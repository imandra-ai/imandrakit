module Log = (val Logger.mk_log_str "x.thread.timer")

let timer_error = Error_kind.make ~name:"TimerError" ()

exception Stop_timer

type t = {
  run_after_s: float -> (unit -> unit) -> unit;
      (** [run_after_s t f] waits [t] seconds and then runs [f] *)
  run_every_s: ?initial:float -> float -> (unit -> unit) -> unit;
      (** [run_every ~initial t f] waits [initial] seconds and then runs [f] every [t] seconds *)
  terminate: unit -> unit;
}

let now_ = Util.mtime_now_s

type task = {
  deadline: float;
  run: unit -> unit;
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
  buf: bytes;  (** Tiny writing buffer, len=1 *)
  mutable t_loop: Thread.t option;
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
    buf = Bytes.make 1 ' ';
    t_loop = None;
  }

type next_step =
  | Wait of float
  | Run of task

let timer_thread_loop_ (self : state) : unit =
  let named = ref false in

  while not (Atomic.get self.closed) do
    (* next thing to do *)
    let next_step =
      Lock.with_lock self.mutex @@ fun () ->
      match T_heap.find_min self.events with
      | None -> Wait 1.
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
    in

    if not !named then (
      (* we run this that late, to make sure the trace collector (if any)
         is setup *)
      named := true;
      Trace.set_thread_name "x.timer"
    );

    match next_step with
    | Wait delay ->
      assert (delay > 0.);
      let _ = Unix.select [ self.p_read ] [] [ self.p_read ] delay in
      (* drain pipe *)
      (try
         while Unix.read self.p_read self.buf 0 1 > 0 do
           ()
         done
       with _ -> ())
    | Run t ->
      (try t.run ()
       with e ->
         Log.err (fun k -> k "error in task:@.%s" (Printexc.to_string e)))
  done

let run_after_s (self : state) t f =
  let deadline = now_ () +. t in
  (* is the new task [f()] scheduled earlier than whatever the
     thread is waiting for? *)
  let is_earlier_than_current_first =
    Lock.with_lock self.mutex (fun () ->
        let task = { deadline; run = f } in
        let is_first =
          match T_heap.find_min self.events with
          | Some task2 -> task2.deadline > deadline
          | _ -> true
        in
        self.events <- T_heap.add self.events task;
        is_first)
  in

  if is_earlier_than_current_first then (
    (* need to wake up the thead, if it's in [Unix.select] *)
    let n = Unix.write_substring self.p_write "t" 0 1 in
    if n = 0 then
      Error.fail ~kind:timer_error "Timer: cannot wake up timer thread"
  )

let terminate_ (self : state) : unit =
  if not (Atomic.exchange self.closed true) then (
    let _n = Unix.write_substring self.p_write "t" 0 1 in
    (* wait for timer thread to terminate *)
    Option.iter Thread.join self.t_loop
  )

let run_every_s (self : state) ?(initial = 0.) period f : unit =
  let rec loop () =
    match f () with
    | () ->
      (* schedule next iteration *)
      run_after_s self period loop
    | exception Stop_timer -> ()
  in
  run_after_s self initial loop

let create () : t =
  let st = create_state () in
  let t_loop = Moonpool.start_thread_on_some_domain timer_thread_loop_ st in
  st.t_loop <- Some t_loop;
  {
    run_after_s = run_after_s st;
    run_every_s = run_every_s st;
    terminate = (fun () -> terminate_ st);
  }

let[@inline] run_every_s (self : t) ?initial s f = self.run_every_s ?initial s f
let[@inline] run_after_s (self : t) s f = self.run_after_s s f
let[@inline] terminate self = self.terminate ()
