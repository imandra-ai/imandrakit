(* we follow
   https://ppxlib.readthedocs.io/en/latest/ppx-for-plugin-authors.html .
   Ported from https://github.com/imandra-ai/cbor-pack/ *)

open Ppxlib
module A = Ast_helper
module B = Ast_builder.Default

let spf = Printf.sprintf

(* name for variables *)
let name_poly_var_ v = spf "_tyreg_poly_%s" v
let lid ~loc s = { loc; txt = Longident.Lident s }
let lid_of_str { txt; loc } = lid ~loc txt

let rec lid_to_str (lid : Longident.t) : string =
  match lid with
  | Longident.Lident s -> s
  | Longident.Ldot (x, s) -> spf "%s.%s" (lid_to_str x) s
  | Longident.Lapply (a, b) -> spf "%s.%s" (lid_to_str a) (lid_to_str b)

let mk_arrow ~loc args body =
  List.fold_right (fun arg bod -> [%type: [%t arg] -> [%t bod]]) args body

(** list literal *)
let rec mk_list ~loc = function
  | [] -> [%expr []]
  | x :: tl -> [%expr [%e x] :: [%e mk_list ~loc tl]]

(** Produce a [Ty_expr.t] *)
let rec tyexpr_of_ty (ty : core_type) : expression =
  let loc = ty.ptyp_loc in
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
      [%expr assert false]
      (*
      let ser_cstor (index : int)
          { pcd_args; pcd_name = cname; pcd_loc = loc; _ } : expression =
        (* constructor identifier *)
        let e_index = A.Exp.constant (A.Const.int index) in
        let lhs, rhs =
          match pcd_args with
          | Pcstr_tuple l ->
            let lhs =
              let pats =
                l
                |> List.mapi (fun i ty ->
                       let loc = ty.ptyp_loc in
                       A.Pat.var { loc; txt = spf "x_%d" i })
              in
              let pat =
                match pats with
                | [] -> None
                | [ x ] -> Some x
                | _ -> Some (A.Pat.tuple pats)
              in
              A.Pat.construct (lid_of_str cname) pat
            in
            let rhs =
              let args =
                l
                |> List.mapi (fun i ty ->
                       let loc = ty.ptyp_loc in
                       tyexpr_of_ty ~ty (A.Exp.ident @@ lid ~loc @@ spf "x_%d" i))
                |> A.Exp.array ~loc
              in
              [%expr
                Encode.cstor enc ~index:[%e e_index]
                  [%e args]]
            in

            lhs, rhs
          | Pcstr_record r ->
            (* variable for the record *)
            let pat_r = A.Pat.var { loc; txt = "r" } in
            let lhs = A.Pat.construct (lid_of_str cname) (Some pat_r) in
            let rhs =
              (* variable for the inline record *)
              let var_r = A.Exp.ident (lid ~loc "r") in
              let args =
                r
                |> List.map (fun { pld_name; pld_loc = _; pld_type; _ } ->
                       let field = A.Exp.field var_r (lid_of_str pld_name) in
                       tyexpr_of_ty field ~ty:pld_type)
                |> A.Exp.array ~loc
              in
              [%expr
                Encode.cstor enc ~index:[%e e_index]
                  [%e args]]
            in
            lhs, rhs
        in

        B.case ~lhs ~guard:None ~rhs
      in
      let branches = List.mapi ser_cstor cstors in
      A.Exp.match_ self branches
  *)
    | Ptype_record labels -> [%expr assert false]
    (* TODO: if some config asks for it, use a pointer-with-metadata
       to pair the tuple with a record descriptor *)
    (*
      let fields =
        labels
        |> List.map (fun { pld_name = field_name; pld_type; _ } ->
               let self_field = A.Exp.field self @@ lid_of_str field_name in
               tyexpr_of_ty self_field ~ty:pld_type)
      in
      [%expr Encode.array enc [%e A.Exp.array fields]]
  *)
  in

  let params =
    param_names d |> List.map name_poly_var_
    |> List.map (fun s -> A.Exp.constant @@ A.Const.string s)
    |> mk_list ~loc
  in

  [%expr
    let open Imandrakit_typereg in
    {
      Ty_def.path = __MODULE__;
      params = [%e params];
      name = [%e A.Exp.constant @@ A.Const.string d.ptype_name.txt];
      decl = [%e decl];
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
let myderiver = Deriving.add "tyreg" ~str_type_decl:impl_generator
