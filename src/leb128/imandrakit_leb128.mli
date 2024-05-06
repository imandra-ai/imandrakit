(** LEB128 encoding and decoding.

 See https://en.wikipedia.org/wiki/LEB128 . *)

module Decode : sig
  val decode_zigzag : int64 -> int64
  val u64 : Byte_slice.t -> int64
  val i64 : Byte_slice.t -> int64

  val int : Byte_slice.t -> int
  (** Like {!i64} but truncates to integer *)
end

module Encode : sig
  val encode_zigzag : int64 -> int64
  val u64 : Byte_buf.t -> int64 -> unit
  val i64 : Byte_buf.t -> int64 -> unit
  val int : Byte_buf.t -> int -> unit
end
