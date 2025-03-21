(** Concise type definition *)

type ty = Ty_expr.t [@@deriving show, eq, yojson, twine]

type record = { fields: (string * ty) list }
[@@deriving show { with_path = false }, eq, yojson, twine]

let map_record ~f { fields } : record =
  { fields = List.map (fun (s, ty) -> s, f ty) fields }

let iter_record ~f { fields } : unit = List.iter (fun (_, ty) -> f ty) fields

type cstor = {
  c: string;  (** Constructor name *)
  args: ty list;
  labels: string list option;
      (** Present for inline records. Length = args.len *)
}
[@@deriving show { with_path = false }, eq, yojson, twine]

let map_cstor ~f (c : cstor) : cstor = { c with args = List.map f c.args }
let iter_cstor ~f c : unit = List.iter f c.args

(** Definition *)
type decl =
  | Alias of ty
  | Alg of cstor list
  | Record of record
[@@deriving show { with_path = false }, eq, yojson, twine]

let map_decl ~f (d : decl) : decl =
  match d with
  | Alias ty -> Alias (f ty)
  | Alg cs -> Alg (List.map (map_cstor ~f) cs)
  | Record r -> Record (map_record ~f r)

let iter_decl ~f (d : decl) : unit =
  match d with
  | Alias ty -> f ty
  | Alg cs -> List.iter (iter_cstor ~f) cs
  | Record r -> iter_record ~f r

type t = {
  path: string;  (** Path *)
  name: string;  (** Name of the type *)
  params: string list;  (** Type parameters *)
  decl: decl;
  unboxed: bool;
}
[@@deriving show { with_path = false }, eq, yojson, twine]

module As_key = struct
  type nonrec t = t

  let equal a b = a.path = b.path && a.name = b.name
  let hash a = Hashtbl.hash (a.path, a.name)
end

let () =
  Imandrakit_twine.Encode.add_cache (module As_key) to_twine_ref;
  Imandrakit_twine.Decode.add_cache of_twine_ref;
  ()

type clique = t list [@@deriving eq, yojson, show, twine]

let map ~f (self : t) : t = { self with decl = map_decl ~f self.decl }
let iter_ty ~f (self : t) = iter_decl ~f self.decl

let compare_by_name (d1 : t) (d2 : t) =
  compare (d1.path, d1.name) (d2.path, d2.name)
