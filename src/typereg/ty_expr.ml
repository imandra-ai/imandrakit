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

let map_shallow ~f (self : t) =
  match self with
  | Var _ -> self
  | Cstor (s, l) -> Cstor (s, List.map f l)
  | Arrow (lbl, a, b) -> Arrow (lbl, f a, f b)
  | Tuple l -> Tuple (List.map f l)

let iter_shallow ~f (self : t) =
  match self with
  | Var _ -> ()
  | Cstor (s, l) -> List.iter f l
  | Arrow (lbl, a, b) ->
    f a;
    f b
  | Tuple l -> List.iter f l

let var v : t = Var v
let cstor c l : t = Cstor (c, l)
let arrow ?label a b : t = Arrow (label, a, b)
let tuple l : t = Tuple l
