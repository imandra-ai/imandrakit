(** Bags.

    An append-optimized sequence of items. *)

type +'a t

val empty : 'a t

val is_empty : _ t -> bool

val cons : 'a -> 'a t -> 'a t

val snoc : 'a t -> 'a -> 'a t

val return : 'a -> 'a t

val append : 'a t -> 'a t -> 'a t

val append_l : 'a t -> 'a list -> 'a t

val length : _ t -> int

val map : ('a -> 'b) -> 'a t -> 'b t

val flat_map : ('a -> 'b t) -> 'a t -> 'b t

val map_opt : ('a -> 'b t) -> 'a option -> 'b t

val iter : ('a -> unit) -> 'a t -> unit

val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val filter : ('a -> bool) -> 'a t -> 'a t

val filter_map : ('a -> 'b option) -> 'a t -> 'b t

val flat_map_l : ('a -> 'b t) -> 'a list -> 'b t

val to_iter : 'a t -> 'a Iter.t

val to_list : 'a t -> 'a list

val of_list : 'a list -> 'a t

val of_iter : 'a Iter.t -> 'a t

module Infix : sig
  val ( @ ) : 'a t -> 'a t -> 'a t
end

include module type of Infix
