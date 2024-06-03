(** Concise type definition *)

type ty = Ty_expr.t [@@deriving show, eq]

type record = { fields: (string * ty) list }
[@@deriving show { with_path = false }, eq]

type cstor = {
  c: string;  (** Constructor name *)
  args: ty list;
  labels: string list option;
      (** Present for inline records. Length = args.len *)
}
[@@deriving show { with_path = false }, eq]

(** Definition *)
type decl =
  | Alias of ty
  | Alg of cstor list
  | Record of record
[@@deriving show { with_path = false }, eq]

type t = {
  path: string;  (** Path *)
  name: string;  (** Name of the type *)
  params: string list;  (** Type parameters *)
  decl: decl;
}
[@@deriving show { with_path = false }, eq]

let compare_by_name (d1 : t) (d2 : t) =
  compare (d1.path, d1.name) (d2.path, d2.name)
