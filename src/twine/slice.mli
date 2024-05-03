(** A simple byte slice *)

type t = private {
  bs: bytes;
  len: int;
}

val create : ?len:int -> bytes -> t
val of_string : string -> t
val get : t -> int -> char
