(** Emit structured logs in the google format

  https://cloud.google.com/logging/docs/structured-logging *)

type json = Yojson.Safe.t

val event_to_json : ?other_fields:(string * json) list -> Log_event.t -> json

val logger :
  ?other_fields:(string * json) list -> out_channel -> Logger.Output.t
