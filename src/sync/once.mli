(** Computed once. Thread safe. *)

type 'a t

val create : (unit -> 'a) -> 'a t
val get : 'a t -> 'a
