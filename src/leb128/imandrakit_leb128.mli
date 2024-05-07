(** LEB128 encoding and decoding.

 See https://en.wikipedia.org/wiki/LEB128 . *)

module Decode : sig
  val decode_zigzag : int64 -> int64

  val skip : Byte_slice.t -> int -> int
  (** [skip slice off] reads an integer at offset [off],
      and returns how many bytes the integer occupies. *)

  val u64 : Byte_slice.t -> int -> int64 * int
  (** [u64 slice off] reads an integer at offset [off],
      and returns a pair [v, n_consumed]. [v] is the read integer,
      [n_consumed] is the number of bytes consumed during reading. *)

  val i64 : Byte_slice.t -> int -> int64 * int

  val int_truncate : Byte_slice.t -> int -> int * int
  (** Like {!i64} but truncates to integer. Returns a pair [v, n_consumed]. *)

  val uint_truncate : Byte_slice.t -> int -> int * int
  (** Like {!u64} but truncates to integer. *)
end

module Encode : sig
  val encode_zigzag : int64 -> int64
  val u64 : Byte_buf.t -> int64 -> unit
  val i64 : Byte_buf.t -> int64 -> unit
  val int : Byte_buf.t -> int -> unit
end
