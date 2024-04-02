(** A really basic bitvector *)

type t

val create : int -> t
val get : t -> int -> bool
val set : t -> int -> unit
val length : t -> int
val of_list : int list -> t
val to_bytes : t -> bytes
val of_bytes : bytes -> t

val all_ones : int -> t
(** [all_ones n] is the bitvector where bits [0, 1, …, n-1] are all true *)

val iter : t -> (int -> unit) -> unit
(** List of bits set to 1 *)

val to_list : t -> int list
(** List of bits set to 1 *)
