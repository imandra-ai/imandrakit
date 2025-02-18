(** Types *)

type offset = int [@@deriving show, eq, ord]
(** An offset in the slice *)

(** A typed offset *)
type 'a offset_for = Offset_for of offset
[@@unboxed] [@@deriving show, eq, ord]

type slice = Byte_slice.t [@@deriving show]
type buf = Byte_buf.t

exception Error of string
