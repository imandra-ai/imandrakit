(** Types *)

type offset = int
(** An offset in the slice *)

type slice = Imandrakit.Byte_slice.t
type buf = Imandrakit.Byte_buf.t

module Tag = struct
  type t =
    | Null
    | True
    | False
    | Int
    | Float
    | String
    | Blob
    | Pointer
    | Array
    | Dict
    | Key
    | Record
    | Constructor
  [@@deriving show { with_path = false }]
end

type tag = Tag.t

module Immediate = struct
  (** An immediate value *)
  type t =
    | Null
    | True
    | False
    | Int of int64
    | Float of float
    | String of slice
    | Blob of slice
    | Pointer of offset
    | Cstor0 of int
  [@@deriving show { with_path = false }]
end

type immediate = Immediate.t

exception Error of string

(**/**)

let[@inline] fail msg = raise (Error msg)
let[@inline] failf msg = Printf.ksprintf fail msg

(**/**)
