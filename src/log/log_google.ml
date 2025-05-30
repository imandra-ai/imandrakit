type json = Yojson.Safe.t

let level_to_severity (l : Log_level.t) : string =
  match l with
  | Logs.App -> "DEFAULT"
  | Logs.Error -> "ERROR"
  | Logs.Warning -> "WARNING"
  | Logs.Info -> "INFO"
  | Logs.Debug -> "DEBUG"

let event_to_json ?(other_fields = []) (ev : Log_event.t) : json =
  let { Log_event.lvl; msg; ts; src; meta } = ev in
  let meta = List.rev_map (fun (k, v) -> k, Log_meta.to_yojson v) meta in
  let msg = Ansi_clean.remove_escape_codes msg in
  let fields =
    ("severity", `String (level_to_severity lvl))
    :: ("timestamp", `String (Timestamp_s.to_string_rfc3339 ~tz_offset_s:0 ts))
    :: ("message", `String msg)
    :: ("imandrax.src", `String src)
    :: meta
  in
  `Assoc (List.rev_append other_fields fields)

let logger ?other_fields oc : Logger.Output.t =
  let oc = Lock_.create oc in
  Logger.Output.to_event ()
    ~flush:(fun () -> Lock_.with_ oc Stdlib.flush)
    ~emit_ev:(fun ev ->
      let json = event_to_json ?other_fields ev in

      try
        (* use a local buffer *)
        let@ buf = Apool.with_resource Logger.Output.buf_pool in
        assert (Buffer.length buf = 0);
        Yojson.Safe.to_buffer buf json;

        (* write buffer's content immediately *)
        let@ oc = Lock_.with_ oc in
        Buffer.output_buffer oc buf;
        output_char oc '\n';
        flush oc
      with exn ->
        Printf.eprintf "log to json in google format: failed with\n%s\n%!"
          (Printexc.to_string exn))
