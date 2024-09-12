(** Run the server *)

type t

val default_port : int
val port : t -> int

val setup : ?port:int -> unit -> unit
(** @raise Error if it cannot bind a socket *)
