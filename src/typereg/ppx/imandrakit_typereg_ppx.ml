(* we follow
   https://ppxlib.readthedocs.io/en/latest/ppx-for-plugin-authors.html .
   Ported from https://github.com/imandra-ai/cbor-pack/ *)

open Ppxlib
module A = Ast_helper

let spf = Printf.sprintf

(* name for variables *)
let name_poly_var_ v = spf "_tyreg_poly_%s" v

(** attribute to define name manually *)
let attr_name =
  Attribute.declare "typereg.name" Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload (estring __))
    (fun x -> x)

let has_attr_unboxed (ty : type_declaration) : bool =
  List.exists (fun a -> a.attr_name.txt = "unboxed") ty.ptype_attributes

let has_attr_twine_use_bytes (ty : core_type) : bool =
  List.exists (fun a -> a.attr_name.txt = "twine.use_bytes") ty.ptyp_attributes

let has_attr_twine_skip_field (ty : core_type) : bool =
  (* indicates that this field is skipped when we serialize *)
  List.exists (fun a -> a.attr_name.txt = "twine_skip_field") ty.ptyp_attributes

let rec lid_to_str (lid : Longident.t) : string =
  match lid with
  | Longident.Lident s -> s
  | Longident.Ldot (x, s) -> spf "%s.%s" (lid_to_str x) s
  | Longident.Lapply (a, b) -> spf "%s.%s" (lid_to_str a) (lid_to_str b)

(** list literal *)
let rec mk_list ~loc = function
  | [] -> [%expr []]
  | x :: tl -> [%expr [%e x] :: [%e mk_list ~loc tl]]

(** Produce a [Ty_expr.t] *)
let rec tyexpr_of_ty (ty : core_type) : expression =
  let loc = ty.ptyp_loc in

  let add_attrs e =
    let attrs =
      List.flatten
        [
          (if has_attr_twine_skip_field ty then
             [ [%expr "twine_skip_field", ""] ]
           else
             []);
          (if has_attr_twine_use_bytes ty then
             [ [%expr "twine.use_bytes", ""] ]
           else
             []);
        ]
    in
    if attrs <> [] then (
      let attrs = mk_list ~loc attrs in
      [%expr Ty_expr.attrs [%e attrs] [%e e]]
    ) else
      e
  in

  add_attrs
  @@
  match ty with
  | [%type: int] -> [%expr Ty_expr.cstor "int" []]
  | [%type: int32] -> [%expr Ty_expr.cstor "int32" []]
  | [%type: int64] -> [%expr Ty_expr.cstor "int64" []]
  | [%type: nativeint] -> [%expr Ty_expr.cstor "nativeint" []]
  | [%type: string] -> [%expr Ty_expr.cstor "string" []]
  | [%type: bytes] -> [%expr Ty_expr.cstor "bytes" []]
  | [%type: bool] -> [%expr Ty_expr.cstor "bool" []]
  | [%type: char] -> [%expr Ty_expr.cstor "char" []]
  | [%type: unit] -> [%expr Ty_expr.cstor "unit" []]
  | [%type: float] -> [%expr Ty_expr.cstor "float" []]
  | [%type: [%t? ty_arg0] option] ->
    [%expr Ty_expr.cstor "option" [ [%e tyexpr_of_ty ty_arg0] ]]
  | [%type: [%t? ty_arg0] list] ->
    [%expr Ty_expr.cstor "list" [ [%e tyexpr_of_ty ty_arg0] ]]
  | [%type: [%t? ty_arg0] array] ->
    [%expr Ty_expr.cstor "array" [ [%e tyexpr_of_ty ty_arg0] ]]
  | { ptyp_desc = Ptyp_var v; ptyp_loc = loc; _ } ->
    (* use function passed as a parameter for each polymorphic argument *)
    let s : string = name_poly_var_ v in
    [%expr Ty_expr.var [%e A.Exp.constant @@ A.Const.string s]]
  | { ptyp_desc = Ptyp_constr (lid, args); ptyp_loc = loc; _ } ->
    [%expr
      Ty_expr.cstor
        [%e A.Exp.constant @@ A.Const.string @@ lid_to_str lid.txt]
        [%e mk_list ~loc @@ List.map tyexpr_of_ty args]]
  | { ptyp_desc = Ptyp_tuple args; ptyp_loc = loc; _ } ->
    [%expr Ty_expr.tuple [%e mk_list ~loc @@ List.map tyexpr_of_ty args]]
  | { ptyp_desc = Ptyp_alias (ty, _); _ } -> tyexpr_of_ty ty
  | { ptyp_desc = Ptyp_variant _; ptyp_loc = loc; _ } ->
    (* TODO *)
    [%expr [%error "Cannot register polymorphic variants yet"]]
  | { ptyp_desc = Ptyp_arrow _; ptyp_loc = loc; _ } ->
    [%expr [%error "Cannot register functions"]]
  | { ptyp_desc = Ptyp_class _ | Ptyp_object _; ptyp_loc = loc; _ } ->
    [%expr [%error "Cannot register objects yet"]]
  | { ptyp_desc = Ptyp_package _; ptyp_loc = loc; _ } ->
    [%expr [%error "Cannot register first-class modules"]]
  | { ptyp_desc = Ptyp_extension _; ptyp_loc = loc; _ } ->
    [%expr [%error "Cannot register type extensions"]]
  | { ptyp_desc = Ptyp_any; ptyp_loc = loc; _ } ->
    [%expr [%error "Cannot register values of type `_`"]]
  | { ptyp_desc = Ptyp_poly _; ptyp_loc = loc; _ } ->
    [%expr [%error "Cannot register values of this type"]]

exception Error_gen of Location.t * string

let error_gen ~loc e = raise (Error_gen (loc, e))

let param_names ty =
  ty.ptype_params
  |> List.map (fun (ty, _) ->
         let loc = ty.ptyp_loc in
         match ty.ptyp_desc with
         | Ptyp_var a -> a
         | Ptyp_any -> error_gen ~loc "Cannot derive tyreg for implicit param"
         | _ -> error_gen ~loc "Cannot derive tyreg for non-variable type")

(** Code for the serialization function for this type decl *)
let tyreg_of_tydecl (d : type_declaration) : expression =
  let loc = d.ptype_loc in

  let decl =
    match d.ptype_kind with
    | Ptype_abstract ->
      (match d.ptype_manifest with
      | Some ty_alias ->
        (* alias, just forward to it *)
        [%expr Alias [%e tyexpr_of_ty ty_alias]]
      | None -> [%expr [%error "cannot derive tyreg for abstract type"]])
    | Ptype_open -> [%expr [%error "cannot derive tyreg for open type"]]
    | Ptype_variant cstors ->
      let conv_cstor (c : constructor_declaration) : expression =
        let args, labels =
          match c.pcd_args with
          | Pcstr_tuple l -> List.map tyexpr_of_ty l, None
          | Pcstr_record r ->
            let args, fields =
              r
              |> List.map (fun (r : label_declaration) ->
                     let ty = tyexpr_of_ty r.pld_type in
                     let lbl =
                       A.Exp.constant @@ A.Const.string r.pld_name.txt
                     in
                     ty, lbl)
              |> List.split
            in
            args, Some fields
        in
        let labels =
          match labels with
          | None -> [%expr None]
          | Some l -> [%expr Some [%e mk_list ~loc l]]
        in
        [%expr
          {
            Ty_def.c = [%e A.Exp.constant @@ A.Const.string c.pcd_name.txt];
            args = [%e mk_list ~loc args];
            labels = [%e labels];
          }]
      in
      let cstors = List.map conv_cstor cstors in
      [%expr Alg [%e mk_list ~loc cstors]]
    | Ptype_record labels ->
      let fields =
        labels
        |> List.map (fun (f : label_declaration) ->
               [%expr
                 [%e A.Exp.constant @@ A.Const.string f.pld_name.txt],
                   [%e tyexpr_of_ty f.pld_type]])
      in
      [%expr Record { fields = [%e mk_list ~loc fields] }]
  in

  let params =
    param_names d |> List.map name_poly_var_
    |> List.map (fun s -> A.Exp.constant @@ A.Const.string s)
    |> mk_list ~loc
  in

  let name =
    match Attribute.get ~mark_as_seen:true attr_name d with
    | None -> d.ptype_name.txt
    | Some s -> s
  in
  let unboxed = has_attr_unboxed d in

  [%expr
    let open Imandrakit_typereg in
    {
      Ty_def.path = __MODULE__;
      params = [%e params];
      name = [%e A.Exp.constant @@ A.Const.string name];
      decl = [%e decl];
      unboxed =
        [%e
          if unboxed then
            [%expr true]
          else
            [%expr false]];
    }]

let generate_impl_ (_rec_flag, type_declarations) : structure_item list =
  let add (tys : type_declaration list) : structure_item =
    let decls = List.map tyreg_of_tydecl tys in
    let loc = Location.none in
    A.Str.eval
      [%expr Imandrakit_typereg.(declare top ~__FILE__) [%e mk_list ~loc decls]]
  in

  [ add type_declarations ]

let generate_impl ~ctxt:_ (rec_flag, type_declarations) =
  try generate_impl_ (rec_flag, type_declarations)
  with Error_gen (loc, msg) ->
    (* emit an error in generated code *)
    let str0 =
      [%stri let () = [%error [%e A.Exp.constant (A.Const.string msg)]]]
    in
    [ str0 ]

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let myderiver = Deriving.add "typereg" ~str_type_decl:impl_generator
