(** Switch to cancel tasks.

    A switch can be flipped to false once, and remains off forever afterwards.
*)

type t [@@deriving show]

val create : ?parent:t -> unit -> t
(** New switch.
    @param parent
      inherit from this switch. It means that the result switches off if the
      parent does, but conversely we can turn the result off without affecting
      the parent. In other words, this switch's lifetime is a subset of the
      parent's lifetime *)

val with_scoped : ?parent:t -> unit -> (t -> 'a) -> 'a
(** [with_scoped ?parent () f] creates a switch using [create ?parent()], run
    [f new_switch], and turns off the switch once [f] returns or raises. *)

val with_on_turn_off : t -> (unit -> unit) -> (unit -> 'a) -> 'a
(** [with_on_turn_off sw cb f] calls [f()]. Within the context where [f()] is
    running, [sw] turning off will trigger [cb()]. Once [f()] has terminated,
    [cb] is disabled and will not be called. *)

val on_turn_off : t -> (unit -> unit) -> unit
(** [on_turn_off sw f] will call [f()] when [sw] is turned off. If [sw] is
    already off then [f()] is called immediately. {b NOTE} [f] really should not
    fail. *)

val is_on : t -> bool
val is_off : t -> bool
val turn_off : ?trace:bool -> t -> unit

val turn_off' : ?trace:bool -> t -> [ `Was_off | `Was_on ]
(** Turn off switch, return previous state *)
