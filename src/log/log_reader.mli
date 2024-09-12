(** Log reader.

    This abstraction allows one to read events back from
    some storage (typically, to display them in the web UI).
*)

class type t = object
  inherit Core_classes.named

  method read_events :
    only_above_level:Logger.level option ->
    filter_meta:(string * string) list ->
    unit ->
    Logger.Log_event.t Iter.t
  (** Read events, in order, from the underlying log source.
          @param only_above_level if [Some lvl], only events more important
          than this level (inclusive) are returned
          @param filter_meta a list of key/value pairs that must match *)
end

val pp : t Fmt.printer

class dummy : t

val accept_ev :
  only_above_level:Logs.level option ->
  filter_meta:(string * string) list ->
  Log_event.t ->
  bool

class of_file_jsonl : string -> t
(** Read from given file as JSONL *)
