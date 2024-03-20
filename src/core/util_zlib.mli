(** Utils around camlzip. *)

val compress : ?buf:Buffer.t -> bytes -> bytes
(** Compress some bytes. *)

val compress_str : ?buf:Buffer.t -> string -> string

val decompress : ?buf:Buffer.t -> bytes -> bytes
(** Decompress the bytes. *)

val decompress_str : ?buf:Buffer.t -> string -> string
