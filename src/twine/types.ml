(** Types *)

type offset = int [@@deriving show]
(** An offset in the slice *)

type slice = Imandrakit.Byte_slice.t [@@deriving show]
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

  let[@inline] bool b =
    if b then
      True
    else
      False

  let[@inline] string (s : string) : t = String (Byte_slice.of_string s)
  let[@inline] blob (s : string) : t = Blob (Byte_slice.of_string s)
  let[@inline] pointer off : t = Pointer off
end

type immediate = Immediate.t

exception Error of string
