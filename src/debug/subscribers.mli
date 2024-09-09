val to_events :
  ?shutdown:('st -> unit) ->
  emit_ev:('st -> Event.t -> unit) ->
  'st ->
  Trace_subscriber.t
(** A subscriber that emits events and calls [emit_ev] for
    each of them *)

val emit : Trace_subscriber.t -> Event.t -> unit
(** Call a callback in the subscriber with the event's data *)
