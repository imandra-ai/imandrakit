(** Simple functional queue *)

type +'a t

val empty : 'a t

val return : 'a -> 'a t

val is_empty : _ t -> bool

val length : _ t -> int

exception Empty

val pop_exn : 'a t -> 'a * 'a t

val push : 'a t -> 'a -> 'a t

val push_l : 'a t -> 'a list -> 'a t

val iter : ('a -> unit) -> 'a t -> unit

val to_iter : 'a t -> 'a Iter.t

val of_list : 'a list -> 'a t

val to_list : 'a t -> 'a list

val append : 'a t -> 'a t -> 'a t

val pp : 'a Fmt.printer -> 'a t Fmt.printer
