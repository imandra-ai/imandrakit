(** A primitive where a thread can block, and
  another thread can unblock it later *)

type t

val create : unit -> t
val wait_block : t -> unit
val unblock : t -> unit
