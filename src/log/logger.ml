open! Log_level

type level = Log_level.t [@@deriving show, eq]

let setup_level ?(default_level = Info) ?debug ?log_level () =
  let lvl =
    match debug, log_level, Sys.getenv_opt "DEBUG" with
    | Some true, _, _ -> Debug
    | _, Some lvl, _ -> lvl
    | Some false, _, _ -> default_level
    | None, None, Some _ -> Debug
    | _ -> default_level
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

  (** New formatter that emits into a buffer, along with a function to get the
      content of the buffer. *)
  let create () : t =
    let buf = Buffer.create 128 in
    let fmt = Fmt.formatter_of_buffer buf in
    Fmt.set_color_tag_handling fmt;
    { fmt; buf }

  let pool : t Apool.t = Apool.create ~clear ~mk_item:create ~max_size:8 ()
end

open struct
  let log_time_pid_tid_ : bool =
    match Sys.getenv_opt "LOG_TIME" with
    | Some ("0" | "false") -> false
    | _ -> true
end

module Output = struct
  type t = {
    emit: Log_event.t -> unit;
    flush: unit -> unit;
  }

  let to_event ~emit_ev ~flush () : t = { emit = emit_ev; flush }
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
    | Logs.Warning -> Fmt.fprintf out "@{<yellow>warn@}"
    | Logs.App -> Fmt.fprintf out "@{<green>app@}");

    let pp_ts out ts =
      if log_time_pid_tid_ then Fmt.fprintf out "|%a" Util.pp_datetime ts
    in

    let pp_pid out () =
      match List.assoc "pid" ev.meta with
      | exception Not_found -> ()
      | Log_meta.Int s -> Fmt.fprintf out "|pid=%d" s
      | Log_meta.String s -> Fmt.fprintf out "|pid=%s" s
      | _ -> ()
    and pp_tid out () =
      match List.assoc "tid" ev.meta with
      | exception Not_found -> ()
      | Log_meta.Int s -> Fmt.fprintf out "|tid=%d" s
      | Log_meta.String s -> Fmt.fprintf out "|tid=%s" s
      | _ -> ()
    in

    Fmt.fprintf out "%a|%s%a%a]@ %a@]@?" pp_ts ev.ts ev.src pp_pid () pp_tid ()
      Util.pp_text_newlines ev.msg;

    Buf_fmt.get_contents buf_fmt

  let to_str ~(emit_str : string -> unit) ~flush () : t =
    {
      emit =
        (fun ev ->
          let s = to_str_ ev in
          emit_str s);
      flush;
    }

  let to_chan ?(autoflush = true) (oc : out_channel) : t =
    let oc = Lock_.create oc in
    to_str
      ~flush:(fun () -> Lock_.with_ oc Stdlib.flush)
      ~emit_str:(fun s ->
        try
          let@ oc = Lock_.with_ oc in
          output_string oc s;
          output_char oc '\n';
          if autoflush then Stdlib.flush oc
        with _ -> Printf.eprintf "logger: failed to log to chan\n%!")
      ()

  let stdout () = to_chan stdout
  let stderr () = to_chan stderr

  let filter_level pred (self : t) : t =
    { emit = (fun ev -> if pred ev.lvl then self.emit ev); flush = self.flush }

  let buf_pool : Buffer.t Apool.t =
    Apool.create ~clear:Buffer.reset
      ~mk_item:(fun () -> Buffer.create 256)
      ~max_size:8 ()

  (** Logger that writes events, one per line, on the given channel. *)
  let to_chan_jsonl ?(autoflush = true) (oc : out_channel) : t =
    let oc = Lock_.create oc in
    to_event ()
      ~flush:(fun () -> Lock_.with_ oc flush)
      ~emit_ev:(fun ev ->
        let json = Log_event.to_yojson ev in

        try
          let@ oc = Lock_.with_ oc in
          ((* use a local buffer *)
           let@ buf = Apool.with_resource buf_pool in
           Buffer.clear buf;
           Yojson.Safe.to_buffer buf json;
           (* write buffer's content immediately *)
           Buffer.output_buffer oc buf);

          output_char oc '\n';
          if autoflush then flush oc
        with exn ->
          Printf.eprintf "log to json chan: failed with %s\n%!"
            (Printexc.to_string exn))
end

type capture_meta_hook = unit -> (string * Log_meta.t) list

open struct
  let capture_meta_hooks : capture_meta_hook list Atomic.t = Atomic.make []

  let add_capture_meta_hook c =
    while
      let l = Atomic.get capture_meta_hooks in
      not (Atomic.compare_and_set capture_meta_hooks l (c :: l))
    do
      ()
    done

  let add_hooks_results l : _ list =
    let hooks = Atomic.get capture_meta_hooks in
    List.fold_left (fun l h -> List.rev_append (h ()) l) l hooks
end

let add_capture_meta_hook = add_capture_meta_hook

open struct
  type rich_tag = RT : 'a Logs.Tag.def * ('a -> Log_meta.t) -> rich_tag

  let rich_tags : rich_tag list Atomic.t = Atomic.make []

  let add_rich_tag def f =
    let st = RT (def, f) in
    while
      let l = Atomic.get rich_tags in
      not (Atomic.compare_and_set rich_tags l (st :: l))
    do
      ()
    done
end

let add_rich_tag = add_rich_tag

type t = {
  active: bool Atomic.t;
  events: Log_event.t Observer.t;  (** Additional subscriptions on events *)
  outputs: Output.t list Atomic.t;
  emit_ev: Log_event.t -> unit;
  emit_fence: unit -> unit;
  reporter: Logs.reporter;  (** As reporter *)
}

let[@inline] as_reporter self = self.reporter
let[@inline] events self = self.events
let[@inline] shutdown self = Atomic.set self.active false

let trace_level_of_level : level -> Trace.Level.t = function
  | Info | App -> Trace.Level.Info
  | Error -> Trace.Level.Error
  | Warning -> Trace.Level.Warning
  | Debug -> Trace.Level.Debug3

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
      (k, Log_meta.String v) :: l)
    tags acc

let to_event_if_ (p : level -> bool) ~emit_ev : Logs.reporter =
  let report src level ~over k msgf =
    if p level then (
      let ts = Util.ptime_now () in
      let src = name_of_src src in

      (* get surrounding tags *)
      let ambient_tags =
        try Log_ctx.get_tags_from_ctx () with _ -> Logs.Tag.empty
      in

      let k (tags : Logs.Tag.set) msg =
        (* remove and convert rich tags *)
        let tags = ref tags in
        let rich_tags =
          List.fold_left
            (fun acc (RT (def, f)) ->
              match Logs.Tag.find def !tags with
              | None -> acc
              | Some x ->
                tags := Logs.Tag.rem def !tags;
                (Logs.Tag.name def, f x) :: acc)
            [] (Atomic.get rich_tags)
        in

        (* gather all metadata in this spot *)
        let meta =
          rich_tags |> add_tags_to_meta !tags
          |> add_tags_to_meta ambient_tags
          |> add_hooks_results
        in

        let meta =
          if log_time_pid_tid_ then
            ("pid", Log_meta.Int (Unix.getpid ()))
            :: ("tid", Log_meta.Int (Thread.id @@ Thread.self ()))
            :: meta
          else
            meta
        in

        let ev = { Log_event.msg; ts; src; lvl = level; meta } in

        (* NOTE: we need to emit to trace here, not in the BG thread, because
           at least the TEF collector needs to know on which thread we are running. *)
        if Trace_core.enabled () then (
          let msg = Ansi_clean.remove_escape_codes msg in
          Trace_core.message ~level:(trace_level_of_level level) msg
            ~data:(fun () ->
              let meta =
                List.map (fun (k, v) -> k, Log_meta.to_trace_data v) meta
              in
              ("src", `String ev.src)
              :: ("lvl", `String (show_level level))
              :: meta)
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

(*
let bg_thread_loop_ (self : t) : unit =
  Trace_core.set_thread_name "logger.bg";
  ignore
    (Unix.sigprocmask Unix.SIG_BLOCK
       [
         Sys.sigterm;
         Sys.sigpipe;
         Sys.sigint;
         Sys.sigchld;
         Sys.sigalrm;
         Sys.sigusr1;
         Sys.sigusr2;
         Sys.sigvtalrm;
       ]
      : _ list);

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
  *)

let fence_ : (unit -> unit) ref = ref ignore
let set_as_fence_ (self : t) : unit = fence_ := self.emit_fence

let to_outputs (outs : Output.t list) : t =
  let active = Atomic.make true in
  let outputs = Atomic.make outs in
  let events = Observer.create () in
  let emit_ev ev =
    (try Observer.emit events ev
     with exn ->
       Printf.eprintf "logger observer failed with %s\n%!"
         (Printexc.to_string exn));
    match Atomic.get outputs with
    | [] -> ()
    | outs ->
      List.iter
        (fun (out : Output.t) ->
          try out.emit ev
          with exn ->
            Printf.eprintf "logger output failed with %s\n%!"
              (Printexc.to_string exn))
        outs
  in
  let reporter =
    to_event_if_
      (fun _ ->
        (* emit event only if we have some outputs or event subscribers *)
        Atomic.get active
        && (Observer.has_subscribers events
           || not (CCList.is_empty (Atomic.get outputs))))
      ~emit_ev
  in

  let emit_fence () =
    let outs = Atomic.get outputs in
    List.iter (fun (out : Output.t) -> out.flush ()) outs
  in

  let self = { active; emit_ev; emit_fence; outputs; reporter; events } in
  self

let[@inline] emit_ev (self : t) ev : unit = self.emit_ev ev
let null () : t = to_outputs []

let setup_ (self : t) : unit =
  Logs.set_reporter self.reporter;
  set_as_fence_ self;
  ()

let with_no_logger () f =
  let old = Logs.reporter () in
  Logs.set_reporter Logs.nop_reporter;
  Fun.protect ~finally:(fun () -> Logs.set_reporter old) f

let setup_logger_to_stdout () =
  let outs = [ Output.stdout () ] in
  let logger = to_outputs outs in
  setup_ logger

let setup_logger_to_stderr () =
  let outs = [ Output.stderr () ] in
  let logger = to_outputs outs in
  setup_ logger

(** Setup a logger that emits into the file specified in ["LOG_FILE"] env, or no
    logger otherwise. *)
let setup_logger_to_LOG_FILE ?filename () k =
  match filename, Sys.getenv_opt "LOG_FILE" with
  | Some file, _ | None, Some file ->
    let@ oc = CCIO.with_out file in
    let outs = [ Output.to_chan oc ] in
    let logger = to_outputs outs in
    setup_ logger;
    k ()
  | _ -> k ()

module type LOG = sig
  include Logs.LOG

  val src : Logs.src
end

let[@inline] fence () = !fence_ ()

let mk_log_str s : (module LOG) =
  let src = Logs.Src.create s in
  Logs.Src.set_level src None;
  (module struct
    include (val Logs.src_log src)

    let src = src
  end)

let all_sources : _ Iter.t =
 fun yield -> Logs.Src.list () |> Iter.of_list |> Iter.iter yield

let all_sources_str : string Iter.t = all_sources |> Iter.map Logs.Src.name

let setup_level_per_source (it : _ Iter.t) : unit =
  let m = Str_map.of_iter it in
  all_sources (fun src ->
      match Str_map.find_opt (Logs.Src.name src) m with
      | None -> ()
      | Some lvl -> Logs.Src.set_level src lvl)

let parse_level_per_source (str : string) : (_ list, string) result =
  try
    let l = String.split_on_char ',' str in
    let src_set = all_sources_str |> Str_set.of_iter in

    let parse_command s =
      match CCString.Split.left ~by:":" s with
      | None -> failwith @@ spf {|Expected "src:level", got %S.|} s
      | Some (src, _) when not (Str_set.mem src src_set) ->
        let suggestions =
          all_sources_str
          |> Iter.filter (fun src' ->
                 CCString.edit_distance ~cutoff:4 src src' <= 2)
          |> Iter.to_rev_list
        in
        failwith
        @@ spf "Unknown logging source %S.%s" src
             (match suggestions with
             | [] -> ""
             | [ s ] -> spf " Did you mean %S?" s
             | l ->
               spf " Did you mean one of %s?"
                 (String.concat ", " @@ List.map (spf "%S") l))
      | Some (src, ("none" | "off")) -> src, None
      | Some (src, lvl) ->
        (match Log_level.parse lvl with
        | None ->
          failwith
          @@ spf
               "Invalid log level %S (valid: \
                app|debug|info|warn|error|none|off)."
               lvl
        | Some _ as lvl -> src, lvl)
    in
    let r = List.map parse_command l in
    Ok r
  with
  | Failure msg -> Error msg
  | exn -> Error (Printexc.to_string exn)

let setup_level_per_source_str str =
  match parse_level_per_source str with
  | Error _ as e -> e
  | Ok l ->
    setup_level_per_source (Iter.of_list l);
    Ok ()
