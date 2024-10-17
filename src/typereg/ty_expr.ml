(** Concise type expression *)

type label_kind =
  | Label
  | Optional
[@@deriving show { with_path = false }, eq, yojson, twine]

type attrs = (string * string) list [@@deriving show, eq, yojson, twine]

type t =
  | Var of string
  | Cstor of string * t list
  | Arrow of (label_kind * string) option * t * t
  | Tuple of t list
  | Attrs of t * attrs
[@@deriving show { with_path = false }, eq, yojson, twine]

let map_shallow ~f (self : t) : t =
  match self with
  | Cstor (s, l) -> Cstor (s, List.map f l)
  | Arrow (lbl, a, b) -> Arrow (lbl, f a, f b)
  | Tuple l -> Tuple (List.map f l)
  | Attrs (ty, attrs) -> Attrs (f ty, attrs)
  | Var _ as view -> view

let iter_shallow ~f (self : t) =
  match self with
  | Var _ -> ()
  | Cstor (_, l) -> List.iter f l
  | Arrow (_, a, b) ->
    f a;
    f b
  | Attrs (ty, _) -> f ty
  | Tuple l -> List.iter f l

let var v : t = Var v
let cstor c l : t = Cstor (c, l)
let arrow ?label a b : t = Arrow (label, a, b)
let tuple l : t = Tuple l

let attrs attrs ty : t =
  if attrs = [] then
    ty
  else
    Attrs (ty, attrs)

let () =
  (* partial hashconsing *)
  let module As_key_partial = struct
    type nonrec t = t

    let rec equal a b =
      match a, b with
      | Var a, Var b -> a = b
      | Cstor (c1, l1), Cstor (c2, l2) -> c1 = c2 && CCList.equal equal l1 l2
      | Tuple l1, Tuple l2 -> CCList.equal equal l1 l2
      | _ -> false

    let hash = Hashtbl.hash
  end in
  Imandrakit_twine.Encode.add_cache (module As_key_partial) to_twine_ref;
  Imandrakit_twine.Decode.add_cache of_twine_ref;
  ()
