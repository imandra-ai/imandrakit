module Log = (val Logger.mk_log_str "x.log-reader")
module Err = Imandrakit_error.Error

class type t =
  object
    inherit Core_classes.named

    method read_events :
      only_above_level:Logger.level option ->
      filter_meta:(string * string) list ->
      unit ->
      Logger.Log_event.t Iter.t
  end

class dummy : t =
  object
    method name = "dummy"
    method read_events ~only_above_level:_ ~filter_meta:_ () = Iter.empty
  end

(** does this event pass the filters? *)
let accept_ev ~only_above_level ~filter_meta (ev : Log_event.t) : bool =
  (match only_above_level with
  | Some lvl -> Log_level.compare ev.lvl lvl <= 0
  | None -> true)
  && List.for_all
       (fun (k, v) ->
         match List.assoc_opt k ev.meta with
         | Some v' -> v = v'
         | None -> false)
       filter_meta

module File_json_l = struct
  type st = {
    file: string;
    buf: Buffer.t;
    only_above_level: Log_level.t option;
    filter_meta: (string * string) list;
  }

  (** read a single line *)
  let read_line (st : st) ~line_idx line : _ option =
    Buffer.clear st.buf;
    match Yojson.Safe.from_string ~buf:st.buf line |> Log_event.of_yojson with
    | Ok ev ->
      if
        accept_ev ~only_above_level:st.only_above_level
          ~filter_meta:st.filter_meta ev
      then
        Some ev
      else
        None
    | Error err ->
      Log.warn (fun k ->
          k "Log_event could not parse line %d of file %S:@ %a" line_idx st.file
            Err.pp err);
      None
    | exception exn ->
      Log.warn (fun k ->
          k "could not parse as JSON line %d of file %S:@ %s" line_idx st.file
            (Printexc.to_string exn));
      None
end

class of_file_jsonl (file : string) : t =
  let open! File_json_l in
  object
    method name = spf "file(%S)" file

    method read_events ~only_above_level ~filter_meta () : _ Iter.t =
      fun yield ->
        let@ ic = CCIO.with_in file in
        let st =
          { file; only_above_level; filter_meta; buf = Buffer.create 64 }
        in
        let it =
          CCIO.read_lines_iter ic |> Iter.zip_i
          |> Iter.filter_map (fun (line_idx, line) ->
                 read_line st ~line_idx line)
        in
        Iter.iter yield it
  end

let pp out (self : #t) = Fmt.fprintf out "<log reader: %s>" self#name
