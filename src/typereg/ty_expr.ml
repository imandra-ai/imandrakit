(** Concise type expression *)

type label_kind =
  | Label
  | Optional
[@@deriving show { with_path = false }, eq, yojson]

type t =
  | Var of string
  | Cstor of string * t list
  | Arrow of (label_kind * string) option * t * t
  | Tuple of t list
[@@deriving show { with_path = false }, eq, yojson]

let var v : t = Var v
let cstor c l : t = Cstor (c, l)
let arrow ?label a b : t = Arrow (label, a, b)
let tuple l : t = Tuple l
