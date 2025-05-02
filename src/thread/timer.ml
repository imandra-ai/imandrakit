module Log = (val Logs.src_log (Logs.Src.create "x.timer"))

let timer_error = Error_kind.make ~name:"TimerError" ()

exception Stop_timer

let now_ = Util.mtime_now_s

type task_state =
  | St_once
  | St_repeat of { period: float }
  | St_cancelled

type task = {
  mutable deadline: float;
  state: task_state Atomic.t;
  mutable run: unit -> unit;
}

module Handle = struct
  type t = task

  let dummy : t =
    { deadline = 0.; state = Atomic.make St_cancelled; run = ignore }

  module For_implementors = struct
    type state = task_state =
      | St_once
      | St_repeat of { period: float }
      | St_cancelled

    let cancelled self = Atomic.get self.state == St_cancelled

    let cancel (self : t) : unit =
      match Atomic.exchange self.state St_cancelled with
      | St_cancelled -> ()
      | St_once | St_repeat _ ->
        (* release memory *)
        self.run <- ignore

    let make ~repeat ~deadline ~task () : t =
      {
        deadline;
        run = task;
        state =
          Atomic.make
            (match repeat with
            | None -> St_once
            | Some period -> St_repeat { period });
      }
  end
end

type t = {
  run_after_s: float -> (unit -> unit) -> Handle.t;
  run_every_s: ?initial:float -> float -> (unit -> unit) -> Handle.t;
  cancel: Handle.t -> unit;
  terminate: unit -> unit;
}

module T_heap = CCHeap.Make (struct
  type t = task

  let leq a b = a.deadline <= b.deadline
end)

type state = {
  mutex: unit Lock.t;
  closed: bool Atomic.t;
  mutable events: T_heap.t;
  p_read: Unix.file_descr;  (** use a fifo to be able to notify the thread *)
  p_write: Unix.file_descr;  (** Notify by writing into fifo *)
  buf4: bytes;  (** Tiny writing buffer, len=4 *)
  mutable t_loop: Thread.t option;  (** Background thread *)
}

let create_state () : state =
  let p_read, p_write = Unix.pipe ~cloexec:true () in
  (* we'll use [select] to wait for the pipe read end to be ready *)
  Unix.set_nonblock p_read;
  {
    mutex = Lock.create ();
    closed = Atomic.make false;
    events = T_heap.empty;
    p_read;
    p_write;
    buf4 = Bytes.create 4;
    t_loop = None;
  }

let wakeup_thread_ (self : state) : unit =
  let n = Unix.write_substring self.p_write "!" 0 1 in
  if n = 0 then
    Error.fail ~kind:timer_error "Timer: cannot wake up timer thread"

let add_task_ (self : state) (task : task) : unit =
  (* is the new task [f()] scheduled earlier than whatever the
     thread is waiting for? In this case the thread needs awakening
     so it can adjust its sleeping delay. *)
  let is_earlier_than_all_current_tasks =
    let@ () = Lock.with_lock self.mutex in
    let is_first =
      match T_heap.find_min self.events with
      | Some task2 -> task2.deadline > task.deadline
      | None -> true
    in
    self.events <- T_heap.add self.events task;
    is_first
  in

  if is_earlier_than_all_current_tasks then
    (* need to wake up the thead, if it's asleep in [Unix.select] *)
    wakeup_thread_ self

type next_step =
  | Wait of float
  | Run of task

let next_step_ (self : state) : next_step =
  let@ () = Lock.with_lock self.mutex in
  match T_heap.find_min self.events with
  | None -> Wait 10.
  | Some task ->
    let now = now_ () in
    let delay = task.deadline -. now in
    if delay <= 1e-6 then (
      (* run task now *)
      let events', _ = T_heap.take_exn self.events in
      self.events <- events';
      Run task
    ) else
      Wait delay

let wait_ (self : state) delay =
  assert (delay > 0.);
  try
    let _ = Unix.select [ self.p_read ] [] [ self.p_read ] delay in
    (* drain pipe *)
    while Unix.read self.p_read self.buf4 0 4 > 0 do
      ()
    done
  with _ -> ()

let run_task_ (self : state) (task : task) : unit =
  let run () : bool =
    try
      task.run ();
      true
    with
    | Stop_timer -> false
    | e ->
      let bt = Printexc.get_raw_backtrace () in
      if not (Atomic.get self.closed) then (
        let err = Error.of_exn ~bt ~kind:timer_error e in
        Log.err (fun k -> k "Error in timer task:@ %a" Error.pp err)
      );
      false
  in

  match Atomic.get task.state with
  | St_cancelled -> ()
  | St_once -> ignore (run () : bool)
  | St_repeat { period } ->
    let continue = run () in

    if continue then (
      task.deadline <- now_ () +. period;
      add_task_ self task
    )

(** The loop running in the background thread *)
let background_thread_loop_ (self : state) : unit =
  let named = ref false in

  while not (Atomic.get self.closed) do
    (* next thing to do *)
    let next_step = next_step_ self in

    if not !named then (
      (* we run this that late, to make sure the trace collector (if any)
         is setup *)
      named := true;
      Trace.set_thread_name "x.timer"
    );

    match next_step with
    | Wait delay -> wait_ self delay
    | Run t -> run_task_ self t
  done

let cancel (_self : t) (h : Handle.t) : unit =
  match Atomic.exchange h.state St_cancelled with
  | St_cancelled -> ()
  | St_once | St_repeat _ ->
    (* release memory *)
    h.run <- ignore

let run_after_s' (self : state) t f : Handle.t =
  if t > 0. then (
    let deadline = now_ () +. t in
    let task = { deadline; state = Atomic.make St_once; run = f } in
    add_task_ self task;
    task
  ) else (
    (try f () with Stop_timer -> ());
    Handle.dummy
  )

let terminate_ (self : state) : unit =
  if not (Atomic.exchange self.closed true) then (
    wakeup_thread_ self;
    (* wait for timer thread to terminate *)
    Option.iter Thread.join self.t_loop
  )

let run_every_s' (self : state) ?initial period f : Handle.t =
  if period <= 0. then invalid_arg "Timer.run_every_s: delay must be > 0.";
  let initial = Option.value ~default:period initial in
  let deadline = now_ () +. initial in
  let task =
    { run = f; state = Atomic.make (St_repeat { period }); deadline }
  in

  let do_schedule =
    if initial = 0. then (
      match f () with
      | () ->
        task.deadline <- now_ () +. period;
        true
      | exception Stop_timer -> false
    ) else
      true
  in

  if do_schedule then add_task_ self task;
  task

let create () : t =
  let st = create_state () in
  let t_loop =
    Moonpool.start_thread_on_some_domain background_thread_loop_ st
  in
  st.t_loop <- Some t_loop;
  {
    run_after_s = run_after_s' st;
    run_every_s = run_every_s' st;
    cancel = Handle.For_implementors.cancel;
    terminate = (fun () -> terminate_ st);
  }

let[@inline] run_after_s' (self : t) s f = self.run_after_s s f

let[@inline] run_after_s self t f : unit =
  ignore (run_after_s' self t f : Handle.t)

let[@inline] run_every_s' (self : t) ?initial s f =
  self.run_every_s ?initial s f

let[@inline] run_every_s self ?initial t f : unit =
  ignore (run_every_s' self ?initial t f : Handle.t)

let[@inline] terminate self = self.terminate ()

let after_s (self : t) t : unit Fut.t =
  let fut, prom = Fut.make () in
  let h =
    run_after_s' self t (fun () -> Fut.fulfill_idempotent prom @@ Ok ())
  in
  Fut.on_result fut (fun r -> if Result.is_error r then cancel self h);
  fut
