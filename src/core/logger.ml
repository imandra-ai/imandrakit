open Log_level
module Sync_queue = Moonpool.Blocking_queue

type level = Log_level.t [@@deriving show, eq]

let setup_level ?debug ?log_level () =
  let lvl =
    match debug, log_level, Sys.getenv_opt "DEBUG" with
    | Some true, _, _ -> Debug
    | _, Some lvl, _ -> lvl
    | Some false, _, _ -> Info
    | None, None, Some _ -> Debug
    | _ -> Info
  in
  Logs.set_level ~all:true (Some lvl)

module Log_event = Log_event

module Buf_fmt = struct
  type t = {
    buf: Buffer.t;
    fmt: Format.formatter;  (** Formatter writing into [buf] *)
  }

  let clear (self : t) : unit =
    Format.pp_print_flush self.fmt ();
    Buffer.reset self.buf

  let get_contents (self : t) =
    Format.fprintf self.fmt "@]@?";
    let m = Buffer.contents self.buf in
    Buffer.reset self.buf;
    m

  (** New formatter that emits into a buffer, along with a function
      to get the content of the buffer. *)
  let create () : t =
    let buf = Buffer.create 128 in
    let fmt = Fmt.formatter_of_buffer buf in
    Fmt.set_color_tag_handling fmt;
    { fmt; buf }

  let pool : t Apool.t = Apool.create ~clear ~mk_item:create ~max_size:8 ()
end

open struct
  let log_time_ : bool =
    match Sys.getenv_opt "LOG_TIME" with
    | Some ("0" | "false") -> false
    | _ -> true
end

module Output = struct
  type t = { emit: Log_event.t -> unit } [@@unboxed]

  let to_event ~emit_ev () : t = { emit = emit_ev }
  let () = Fmt.set_color_default true

  let to_str_ (ev : Log_event.t) : string =
    (* acquire a formatter from the pool *)
    let@ buf_fmt = Apool.with_resource Buf_fmt.pool in
    let out = buf_fmt.fmt in

    Fmt.fprintf out "@[<2>[";
    (match ev.lvl with
    | Logs.Debug -> Fmt.fprintf out "@{<Blue>debug@}"
    | Logs.Info -> Fmt.fprintf out "@{<cyan>info@}"
    | Logs.Error -> Fmt.fprintf out "@{<Red>error@}"
    | Logs.Warning -> Fmt.fprintf out "@{<yellow>warning@}"
    | Logs.App -> Fmt.fprintf out "@{<green>app@}");

    let pp_ts out ts =
      if log_time_ then
        Fmt.fprintf out ":%a" Util.pp_datetime ts
      else
        ()
    in

    Fmt.fprintf out ":%s%a]@ %a@]@?" ev.src pp_ts ev.ts Util.pp_text_newlines
      ev.msg;

    Buf_fmt.get_contents buf_fmt

  let to_str ~(emit_str : string -> unit) () : t =
    {
      emit =
        (fun ev ->
          let s = to_str_ ev in
          emit_str s);
    }

  let to_chan (oc : out_channel) : t =
    to_str
      ~emit_str:(fun s ->
        try
          output_string oc s;
          output_char oc '\n';
          flush oc
        with _ -> Printf.eprintf "logger: failed to log to chan\n%!")
      ()

  let stdout () = to_chan stdout
  let stderr () = to_chan stderr

  let filter_level pred (self : t) : t =
    { emit = (fun ev -> if pred ev.lvl then self.emit ev) }

  let buf_pool : Buffer.t Apool.t =
    Apool.create ~clear:Buffer.reset
      ~mk_item:(fun () -> Buffer.create 64)
      ~max_size:8 ()

  (** Logger that writes events, one per line, on the given channel. *)
  let to_chan_jsonl (oc : out_channel) : t =
    to_event () ~emit_ev:(fun ev ->
        let json = Log_event.to_yojson ev in

        try
          ((* use a local buffer *)
           let@ buf = Apool.with_resource buf_pool in
           Buffer.clear buf;
           Yojson.Safe.to_buffer buf json;
           (* write buffer's content immediately *)
           Buffer.output_buffer oc buf);

          output_char oc '\n';
          flush oc
        with exn ->
          Printf.eprintf "log to json chan: failed with %s\n%!"
            (Printexc.to_string exn))
end

type task =
  | T_fence of { wakeup: unit Moonpool.Fut.promise }
  | T_emit of Log_event.t

type t = {
  q: task Sync_queue.t;
  events: Log_event.t Observer.t;
  outputs: Output.t list Atomic.t;
  reporter: Logs.reporter;
  mutable bg_thread: Thread.t option;
}

let[@inline] as_reporter self = self.reporter
let[@inline] events self = self.events

let shutdown self =
  Sync_queue.close self.q;
  Option.iter Thread.join self.bg_thread

let add_output self out : unit =
  while
    let l = Atomic.get self.outputs in
    not (Atomic.compare_and_set self.outputs l (out :: l))
  do
    ()
  done

let name_of_src (src : Logs.src) : string =
  let src = Logs.Src.name src in
  if src = "application" then
    "app"
  else
    src

let add_tags_to_meta (tags : Logs.Tag.set) acc : _ list =
  Logs.Tag.fold
    (fun (Logs.Tag.V (t_def, v)) l ->
      let k = Logs.Tag.name t_def in
      let v = Fmt.to_string (Logs.Tag.printer t_def) v in
      (k, v) :: l)
    tags acc

let to_event_if_ (p : level -> bool) ~emit_ev : Logs.reporter =
  let report src level ~over k msgf =
    if p level then (
      let ts = Util.ptime_now () in
      let src = name_of_src src in
      (* get surrounding tags *)
      let ambient_tags = Log_ctx.get_tags_from_ctx () in

      let k (tags : Logs.Tag.set) msg =
        let meta =
          [] |> add_tags_to_meta tags |> add_tags_to_meta ambient_tags
        in
        let ev = { Log_event.msg; ts; src; lvl = level; meta } in

        (* NOTE: we need to emit to trace here, not in the BG thread, because
           at least the TEF collector needs to know on which thread we are running. *)
        if Trace_core.enabled () then (
          let msg = Ansi_clean.remove_escape_codes msg in
          Trace_core.message msg ~data:(fun () ->
              let meta = List.map (fun (k, v) -> k, `String v) meta in
              [ "src", `String ev.src; "lvl", `String (show_level level) ]
              @ meta)
        );

        emit_ev ev;
        over ();
        k ()
      in

      msgf (fun ?header:_ ?(tags = Logs.Tag.empty) fmt ->
          Format.kasprintf (k tags) fmt)
    ) else (
      over ();
      k ()
    )
  in
  { Logs.report }

let bg_thread_loop_ (self : t) : unit =
  Trace_core.set_thread_name "logger.bg";
  let local_q = Queue.create () in
  try
    let process_task = function
      | T_fence { wakeup } -> Moonpool.Fut.fulfill wakeup @@ Ok ()
      | T_emit ev ->
        let outs = Atomic.get self.outputs in
        Observer.emit self.events ev;
        List.iter (fun (out : Output.t) -> out.emit ev) outs
    in

    while true do
      (* batch transfer *)
      Sync_queue.transfer self.q local_q;
      Queue.iter process_task local_q;
      Queue.clear local_q
    done
  with Sync_queue.Closed -> ()

let fence_ : (unit -> unit Moonpool.Fut.t) ref =
  ref (fun () -> Moonpool.Fut.return ())

let to_outputs (outs : Output.t list) : t =
  let outputs = Atomic.make outs in
  let events = Observer.create () in
  let q = Sync_queue.create () in
  let reporter =
    to_event_if_
      (fun _ ->
        (* emit event only if we have some outputs or event subscribers *)
        Observer.has_subscribers events
        || not (CCList.is_empty (Atomic.get outputs)))
      ~emit_ev:(fun ev ->
        try Sync_queue.push q (T_emit ev) with Sync_queue.Closed -> ())
  in

  let fence () =
    let fut, prom = Moonpool.Fut.make () in
    (try Sync_queue.push q (T_fence { wakeup = prom })
     with Sync_queue.Closed -> Moonpool.Fut.fulfill prom @@ Ok ());
    fut
  in

  fence_ := fence;
  let self = { q; outputs; reporter; events; bg_thread = None } in
  self.bg_thread <- Some (Thread.create bg_thread_loop_ self);
  self

let null () : t = to_outputs []

let with_no_logger () f =
  let old = Logs.reporter () in
  Logs.set_reporter Logs.nop_reporter;
  Fun.protect ~finally:(fun () -> Logs.set_reporter old) f

let setup_logger_to_stdout () =
  let outs = [ Output.stdout () ] in
  let reporter = to_outputs outs in
  Logs.set_reporter (as_reporter reporter)

let setup_logger_to_stderr () =
  let outs = [ Output.stderr () ] in
  let reporter = to_outputs outs in
  Logs.set_reporter (as_reporter reporter)

(** Setup a logger that emits into the file specified in ["LOG_FILE"] env,
    or no logger otherwise. *)
let setup_logger_to_LOG_FILE ?filename () k =
  match filename, Sys.getenv_opt "LOG_FILE" with
  | Some file, _ | None, Some file ->
    let@ oc = CCIO.with_out file in
    let outs = [ Output.to_chan oc ] in
    Logs.set_reporter (to_outputs outs |> as_reporter);
    k ()
  | _ -> k ()

module type LOG = sig
  include Logs.LOG

  val src : Logs.src
end

let fence () =
  let fut = !fence_ () in
  Moonpool.Fut.wait_block_exn fut

let mk_log_str s : (module LOG) =
  let src = Logs.Src.create s in
  Logs.Src.set_level src None;
  (module struct
    include (val Logs.src_log src)

    let src = src
  end)

let all_sources : _ Iter.t =
 fun yield -> Logs.Src.list () |> Iter.of_list |> Iter.iter yield

let setup_level_per_source (it : _ Iter.t) : unit =
  let m = Str_map.of_iter it in
  all_sources (fun src ->
      match Str_map.find_opt (Logs.Src.name src) m with
      | None -> ()
      | Some lvl -> Logs.Src.set_level src lvl)
