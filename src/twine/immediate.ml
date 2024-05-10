open Types

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

let null = Null
let[@inline] string (s : string) : t = String (Byte_slice.of_string s)
let[@inline] blob (s : string) : t = Blob (Byte_slice.of_string s)
let[@inline] pointer off : t = Pointer off
let[@inline] int64 x = Int x
let[@inline] int x = Int (Int64.of_int x)
let[@inline] float x = Float x
let[@inline] cstor0 ~index : t = Cstor0 index

let[@inline] bool b =
  if b then
    True
  else
    False
