(** Reactive variable.

    A reactive variable holds state, and can change state over time.
    One can subscribe to changes or just get the current value. *)

type 'a t

val return : 'a -> 'a t
(** A frozen variable with the given content. *)

val get : 'a t -> 'a
(** Get the current value. *)

val pp : 'a Fmt.printer -> 'a t Fmt.printer

exception Frozen

val mk_var : 'a -> 'a t
(** Make a new variable with the given initial value *)

val set : 'a t -> 'a -> unit
(** Set the value. This will trigger all the callbacks registered via {!on_change}.
    @raise Frozen if the variable is frozen. *)

val update : 'a t -> f:('a -> 'a * 'b) -> 'b
(** Modifies content and return side value.
    @raise Frozen if the variable is frozen. *)

val freeze : _ t -> unit
(** Prevent further mutations. Idempotent. *)

(** {2 Register to changes} *)

type 'a cb = 'a option -> unit

val on_change : 'a t -> 'a cb -> unit
(** [on_change rv f] subscribes [f] to be called whenever [rv] changes.
    When [rv] is set to a new value [x], [f (Some x)] is called.
    When [rv] is frozen, [f None] is called (and then [f] will never
    be called again).

    If [rv] is already frozen then [f None] will be called immediately.
*)

(** {2 Combinators} *)

val map : f:('a -> 'b) -> 'a t -> 'b t
val map2 : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val flatten : 'a t t -> 'a t
val flat_map : f:('a -> 'b t) -> 'a t -> 'b t
val of_fut : 'a Fut.t -> 'a option t
val of_fut_or_error : 'a Fut.t -> 'a Fut.or_error option t

val pick : 'a t list -> 'a t
(** Pick a value from a list of variables. The result is a variable that is always as
    up-to-date as any of the inputs.
    @raise Invalid_argument if the list is empty. *)

val cutoff : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t
(** [cutoff v] produces a new variable that always has the same
    value as [v], but only emits "on change" events when the new
    value differs from the old one.
    @param eq equality function (default is polymorphic equality) *)

module Monoid : sig
  type 'a t = 'a * ('a -> 'a -> 'a)

  val empty : 'a t -> 'a
  val merge : 'a t -> 'a -> 'a -> 'a
end

val monoid_merge : 'm Monoid.t -> f:('a -> 'm) -> 'a t -> 'a t -> 'm t
(** [monoid_merge m ~f v1 v2] maps each variable using [f]
    and then merges the results using the given monoid. *)

val monoid_merge_l : 'm Monoid.t -> f:('a -> 'm) -> 'a t list -> 'm t
(** [monoid_merge m ~f rvs ] maps each variable using [f]
    and then merges the results using the given monoid. *)

module Infix : sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
end

include module type of Infix
