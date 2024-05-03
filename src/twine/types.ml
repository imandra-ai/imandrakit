(** Types *)

type tag =
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

exception Error of string

type offset = int
(** An offset in the slice *)

type record_descriptor = { fs: string array }
(** The description of a record type. Should be added only once to the output. *)

type cstor_descriptor = {
  ar: int;
  fs: string array option;
}
(** Description of a constructor *)

type sum_type_descriptor = { cs: cstor_descriptor array  (** Constructors *) }
(** Description of a sum type. Should be added only once to the output *)

(**/**)

let[@inline] fail msg = raise (Error msg)
let[@inline] failf msg = Printf.ksprintf fail msg

(**/**)
