(** Types *)

type offset = int [@@deriving show, eq, ord]
(** An offset in the slice *)

type slice = Byte_slice.t [@@deriving show]
type buf = Byte_buf.t

exception Error of string
