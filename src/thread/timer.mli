(** Timer.

    Keep track of multiple timers, using a background thread. The timer is
    thread-safe. It runs callbacks from within its own background thread. *)

module Handle : sig
  type t

  val dummy : t

  module For_implementors : sig
    val cancel : t -> unit
    val cancelled : t -> bool

    val make :
      repeat:float option -> deadline:float -> task:(unit -> unit) -> unit -> t
  end
end

type t = {
  run_after_s: float -> (unit -> unit) -> Handle.t;
  run_every_s: ?initial:float -> float -> (unit -> unit) -> Handle.t;
  cancel: Handle.t -> unit;
  terminate: unit -> unit;
}

exception Stop_timer

val timer_error : Error_kind.t

val run_after_s : t -> float -> (unit -> unit) -> unit
(** [run_after_s t f] waits [t] seconds and then runs [f] *)

val run_after_s' : t -> float -> (unit -> unit) -> Handle.t

val after_s : t -> float -> unit Fut.t
(** [after_s timer t] returns a future that resolves in [t] seconds. *)

val run_every_s : t -> ?initial:float -> float -> (unit -> unit) -> unit
(** [run_every ~initial t f] waits [initial] seconds and then runs [f()] every
    [t] seconds. [f ()] can raise [Stop_timer] to stop the loop. *)

val run_every_s' : t -> ?initial:float -> float -> (unit -> unit) -> Handle.t

val cancel : t -> Handle.t -> unit
(** Cancel timer *)

val create : unit -> t
(** New timer that uses a background thread *)

val terminate : t -> unit
(** Stop the timer *)
