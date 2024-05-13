(** A simple byte slice *)

type t = {
  bs: bytes;
  mutable off: int;
  mutable len: int;
}
[@@deriving show]

val create : ?off:int -> ?len:int -> bytes -> t
val clear : t -> unit
val of_string : string -> t
val len : t -> int
val get : t -> int -> char
val set : t -> int -> char -> unit
val consume : t -> int -> unit
val contents : t -> string
val sub : t -> int -> int -> t
