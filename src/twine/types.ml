(** Types *)

type offset = int [@@deriving show]
(** An offset in the slice *)

type slice = Imandrakit.Byte_slice.t [@@deriving show]
type buf = Imandrakit.Byte_buf.t

exception Error of string
