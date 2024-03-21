(** Timer.

    Keep track of multiple timers, using a background thread.
    The timer is thread-safe. It runs callbacks from within
    its own background thread.
*)

type t = {
  run_after_s: float -> (unit -> unit) -> unit;
      (** [run_after_s t f] waits [t] seconds and then runs [f] *)
  run_every_s: ?initial:float -> float -> (unit -> unit) -> unit;
      (** [run_every ~initial t f] waits [initial] seconds and then runs [f] every [t] seconds *)
  terminate: unit -> unit;
}

exception Stop_timer

val run_after_s : t -> float -> (unit -> unit) -> unit
(** [run_after_s t f] waits [t] seconds and then runs [f] *)

val run_every_s : t -> ?initial:float -> float -> (unit -> unit) -> unit
(** [run_every ~initial t f] waits [initial] seconds and
    then runs [f()] every [t] seconds.
    [f ()] can raise [Stop_timer] to stop the loop. *)

val create : unit -> t
val terminate : t -> unit
