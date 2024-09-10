(** Callbacks the emit an event *)
module To_events_callbacks : sig
  (** State and callback *)
  type st =
    | St : {
        st: 'a;
        emit_ev: 'a -> Event.t -> unit;
      }
        -> st

  include Trace_subscriber.Callbacks.S with type st := st
end

val to_events : emit_ev:('st -> Event.t -> unit) -> 'st -> Trace_subscriber.t
(** A subscriber that emits events and calls [emit_ev] for
    each of them *)

val emit : Trace_subscriber.t -> Event.t -> unit
(** Call a callback in the subscriber with the event's data *)
