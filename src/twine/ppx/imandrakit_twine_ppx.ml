(* we follow
   https://ppxlib.readthedocs.io/en/latest/ppx-for-plugin-authors.html .

   Ported from https://github.com/imandra-ai/cbor-pack/ *)

open Ppxlib
module A = Ast_helper
module B = Ast_builder.Default

let spf = Printf.sprintf

let ser_name_of_ty_name (ty_name : string) : string =
  if ty_name = "t" then
    "to_twine"
  else
    ty_name ^ "_to_twine"

let ser_name_ref_of_ty_name (ty_name : string) : string =
  ser_name_of_ty_name ty_name ^ "_ref"

let deser_name_of_ty_name (ty_name : string) : string =
  if ty_name = "t" then
    "of_twine"
  else
    ty_name ^ "_of_twine"

let deser_name_ref_of_ty_name (ty_name : string) : string =
  deser_name_of_ty_name ty_name ^ "_ref"

(* name for variables *)
let name_poly_var_ v = spf "_twine_poly_%s" v

let rec map_lid ~f (lid : Longident.t) : Longident.t =
  match lid with
  | Longident.Lident name -> Longident.Lident (f name)
  | Longident.Lapply (a, b) -> Longident.Lapply (a, map_lid ~f b)
  | Longident.Ldot (m, a) -> Longident.Ldot (m, f a)

let ser_name_of_lid = map_lid ~f:ser_name_of_ty_name
let dec_name_of_lid = map_lid ~f:deser_name_of_ty_name

let ser_name_ref_of_ty (ty : type_declaration) : string =
  let ty_name = ty.ptype_name.txt in
  ser_name_ref_of_ty_name ty_name

let ser_name_of_ty (ty : type_declaration) : string =
  let ty_name = ty.ptype_name.txt in
  ser_name_of_ty_name ty_name

let deser_name_of_ty (ty : type_declaration) : string =
  let ty_name = ty.ptype_name.txt in
  deser_name_of_ty_name ty_name

let deser_name_ref_of_ty (ty : type_declaration) : string =
  let ty_name = ty.ptype_name.txt in
  deser_name_ref_of_ty_name ty_name

let lid ~loc s = { loc; txt = Longident.Lident s }
let lid_of_str { txt; loc } = lid ~loc txt

(* list literal *)
let rec mk_list ~loc = function
  | [] -> [%expr []]
  | x :: tl -> [%expr [%e x] :: [%e mk_list ~loc tl]]

let rec mk_list_pat ~loc = function
  | [] -> [%pat? []]
  | x :: tl -> [%pat? [%p x] :: [%p mk_list_pat ~loc tl]]

let mk_lambda ~loc args body =
  List.fold_right
    (fun arg bod -> [%expr fun [%p A.Pat.var { loc; txt = arg }] -> [%e bod]])
    args body

let mk_arrow ~loc args body =
  List.fold_right (fun arg bod -> [%type: [%t arg] -> [%t bod]]) args body

(* attribute to define serializer manually *)
let attr_encode =
  Attribute.declare "twine.encode" Attribute.Context.core_type
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

(* attribute to force a [string] to encode into a blob *)
let attr_use_bytes =
  Attribute.declare "twine.use_bytes" Attribute.Context.core_type
    Ast_pattern.(pstr nil)
    ()

let attr_decode =
  Attribute.declare "twine.decode" Attribute.Context.core_type
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

(* apply [Twine.Encode.t -> 'a -> offset] function *)
let apply_encode ~loc e_encode e : expression =
  [%expr
    let e = [%e e] in
    ([%e e_encode] enc e : Imandrakit_twine.immediate)]

(* apply [Decode.t -> offset -> 'a] *)
let apply_decode ~loc e_deser p : expression =
  [%expr [%e e_deser] dec ([%e p] : Imandrakit_twine.offset)]

let is_some_ = function
  | Some _ -> true
  | None -> false

(** Produce an immediate value *)
let rec immediate_expr_of_ty (e : expression) ~(ty : core_type) : expression =
  let loc = ty.ptyp_loc in
  let apply_encode = apply_encode ~loc in
  let by_encode ser = apply_encode ser e in
  match ty with
  | _ when is_some_ (Attribute.get ~mark_as_seen:false attr_encode ty) ->
    (* user provided serializer *)
    let e_ser =
      match Attribute.get ~mark_as_seen:true attr_encode ty with
      | None -> assert false
      | Some h -> h
    in
    apply_encode e_ser e
  | [%type: int] -> [%expr Imandrakit_twine.Immediate.Int (Int64.of_int [%e e])]
  | [%type: int32] ->
    [%expr Imandrakit_twine.Immediate.Int (Int64.of_int32 [%e e])]
  | [%type: int64] -> [%expr Imandrakit_twine.Immediate.Int [%e e]]
  | [%type: nativeint] ->
    [%expr Imandrakit_twine.Immediate.Int (Int64.of_nativeint [%e e])]
  | [%type: string] ->
    if is_some_ @@ Attribute.get ~mark_as_seen:false attr_use_bytes ty then
      [%expr Imandrakit_twine.Immediate.blob [%e e]]
    else
      [%expr Imandrakit_twine.Immediate.string [%e e]]
  | [%type: bytes] ->
    let e = [%expr Bytes.unsafe_to_string [%e e]] in
    if is_some_ @@ Attribute.get ~mark_as_seen:false attr_use_bytes ty then
      [%expr Imandrakit_twine.Immediate.blob [%e e]]
    else
      [%expr Imandrakit_twine.Immediate.string [%e e]]
  | [%type: bool] -> [%expr Imandrakit_twine.Immediate.bool [%e e]]
  | [%type: char] ->
    [%expr Imandrakit_twine.Immediate.Int (Int64.of_int @@ Char.code [%e e])]
  | [%type: unit] -> [%expr Imandrakit_twine.Immediate.Null]
  | [%type: float] -> [%expr Imandrakit_twine.Immediate.Float [%e e]]
  | [%type: [%t? ty_arg0] option] ->
    [%expr
      match [%e e] with
      | None -> Imandrakit_twine.Immediate.Null
      | Some x ->
        Imandrakit_twine.(
          Encode.list enc [ [%e immediate_expr_of_ty [%expr x] ~ty:ty_arg0] ])]
  | [%type: [%t? ty_arg0] list] ->
    apply_encode [%expr Imandrakit_twine.Encode.array_iter]
      [%expr
        fun yield ->
          List.iter
            (fun x -> yield [%e immediate_expr_of_ty ~ty:ty_arg0 [%expr x]])
            [%e e]]
  | [%type: [%t? ty_arg0] array] ->
    [%expr
      let arr = [%e e] in
      (Imandrakit_twine.Encode.array_init enc (Array.length arr) (fun i ->
           let v = Array.unsafe_get arr i in
           [%e immediate_expr_of_ty ~ty:ty_arg0 [%expr v]])
        : Imandrakit_twine.immediate)]
  | { ptyp_desc = Ptyp_var v; ptyp_loc = loc; _ } ->
    (* use function passed as a parameter for each polymorphic argument *)
    let s = A.Exp.ident @@ lid ~loc @@ name_poly_var_ v in
    by_encode s
  | { ptyp_desc = Ptyp_constr (lid, args); ptyp_loc = loc; _ } ->
    (* find function for this type and apply it to args, themselves functions *)
    let f = A.Exp.ident { loc; txt = ser_name_of_lid lid.txt } in
    let args =
      args
      |> List.map (fun ty ->
             let ser =
               [%expr fun enc x -> [%e immediate_expr_of_ty [%expr x] ~ty]]
             in
             Nolabel, ser)
    in
    by_encode
      (if args = [] then
        f
      else
        A.Exp.apply f args)
  | { ptyp_desc = Ptyp_tuple args; ptyp_loc = loc; _ } ->
    let ser_args =
      args
      |> List.mapi (fun i ty ->
             let x_i = A.Exp.ident (lid ~loc @@ spf "x_%d" i) in
             immediate_expr_of_ty x_i ~ty)
    in
    (* [let (x1,...,xn) = e in …] *)
    let vbs =
      let tup_pat =
        A.Pat.tuple
        @@ List.mapi
             (fun i ty ->
               let loc = ty.ptyp_loc in
               A.Pat.var { loc; txt = spf "x_%d" i })
             args
      in
      [ A.Vb.mk tup_pat e ]
    in
    let body =
      [%expr Imandrakit_twine.Encode.(array enc [%e A.Exp.array ~loc ser_args])]
    in
    A.Exp.let_ Nonrecursive vbs body
  | { ptyp_desc = Ptyp_alias (ty, _); _ } -> immediate_expr_of_ty e ~ty
  | { ptyp_desc = Ptyp_variant _; ptyp_loc = loc; _ } ->
    (* TODO *)
    [%expr [%error "Cannot serialize polymorphic variants yet"]]
  | { ptyp_desc = Ptyp_arrow _; ptyp_loc = loc; _ } ->
    [%expr [%error "Cannot serialize functions"]]
  | { ptyp_desc = Ptyp_class _ | Ptyp_object _; ptyp_loc = loc; _ } ->
    [%expr [%error "Cannot serialize objects yet"]]
  | { ptyp_desc = Ptyp_package _; ptyp_loc = loc; _ } ->
    [%expr [%error "Cannot serialize first-class modules"]]
  | { ptyp_desc = Ptyp_extension _; ptyp_loc = loc; _ } ->
    [%expr [%error "Cannot serialize type extensions"]]
  | { ptyp_desc = Ptyp_any; ptyp_loc = loc; _ } ->
    [%expr [%error "Cannot serialize values of type `_`"]]
  | { ptyp_desc = Ptyp_poly _; ptyp_loc = loc; _ } ->
    [%expr [%error "Cannot serialize values of this type"]]

(* produce an expression that deserializes offset represented by [e].
   In scope: [dec]. *)
let rec decode_expr_of_ty (e : expression) ~(ty : core_type) : expression =
  let loc = ty.ptyp_loc in
  let by_full_dec ser = apply_decode ~loc ser e in
  match ty with
  | _ when is_some_ (Attribute.get ~mark_as_seen:false attr_decode ty) ->
    (* custom deser *)
    let e_deser =
      match Attribute.get ~mark_as_seen:true attr_decode ty with
      | None -> assert false
      | Some h -> h
    in
    apply_decode ~loc e_deser e
  | [%type: int] -> by_full_dec [%expr Imandrakit_twine.Decode.int_truncate]
  | [%type: int32] ->
    [%expr Imandrakit_twine.Decode.int_truncate dec [%e e] |> Int32.of_int]
  | [%type: int64] -> [%expr Imandrakit_twine.Decode.int64 dec [%e e]]
  | [%type: nativeint] ->
    [%expr Imandrakit_twine.Decode.int64 dec [%e e] |> Int64.to_nativeint]
  | [%type: string] ->
    if is_some_ @@ Attribute.get ~mark_as_seen:false attr_use_bytes ty then
      by_full_dec [%expr Imandrakit_twine.Decode.blob]
    else
      by_full_dec [%expr Imandrakit_twine.Decode.string]
  | [%type: bytes] ->
    [%expr
      [%e
        if is_some_ @@ Attribute.get ~mark_as_seen:false attr_use_bytes ty then
          by_full_dec [%expr Imandrakit_twine.Decode.blob]
        else
          by_full_dec [%expr Imandrakit_twine.Decode.string]]
      |> Bytes.unsafe_of_string]
  | [%type: bool] -> by_full_dec [%expr Imandrakit_twine.Decode.bool]
  | [%type: char] ->
    [%expr Imandrakit_twine.Decode.int_truncate dec [%e e] |> Char.chr]
  | [%type: unit] -> by_full_dec [%expr Imandrakit_twine.Decode.null]
  | [%type: float] -> by_full_dec [%expr Imandrakit_twine.Decode.float]
  | [%type: [%t? ty_arg0] option] ->
    [%expr
      match Imandrakit_twine.Decode.(read dec [%e e]) with
      | Null -> None
      | Array c ->
        let x =
          [%e
            decode_expr_of_ty ~ty:ty_arg0
              [%expr Imandrakit_twine.Decode.Array_cursor.current c]]
        in
        Some x
      | _ ->
        Imandrakit_twine.Decode.fail "expected an option (null or 1-item list)"]
  | [%type: [%t? ty_arg0] list] ->
    [%expr
      Imandrakit_twine.Decode.(
        array dec [%e e]
        |> Array_cursor.to_list_of (fun x ->
               [%e decode_expr_of_ty [%expr x] ~ty:ty_arg0]))]
  | [%type: [%t? ty_arg0] array] ->
    [%expr
      Imandrakit_twine.Decode.(
        array dec [%e e]
        |> Array_cursor.to_array_of (fun offset ->
               [%e decode_expr_of_ty ~ty:ty_arg0 [%expr offset]]))]
  | { ptyp_desc = Ptyp_var v; ptyp_loc = loc; _ } ->
    (* use function passed as a parameter for each polymorphic argument *)
    let s = A.Exp.ident @@ lid ~loc @@ name_poly_var_ v in
    by_full_dec s
  | { ptyp_desc = Ptyp_constr (lid, args); ptyp_loc = loc; _ } ->
    (* find function for this type and apply it to args, themselves functions *)
    let f = A.Exp.ident { loc; txt = dec_name_of_lid lid.txt } in
    let args =
      args
      |> List.map (fun ty ->
             let f =
               [%expr fun dec x -> [%e decode_expr_of_ty [%expr x] ~ty]]
             in
             Nolabel, f)
    in
    by_full_dec
      (if args = [] then
        f
      else
        A.Exp.apply f args)
  | { ptyp_desc = Ptyp_tuple args; ptyp_loc = loc; _ } ->
    let arity = List.length args in

    let tup_vars, deser_args =
      args
      |> List.mapi (fun i ty_i ->
             let name_var = { loc; txt = spf "x_%d" i } in
             let p_var = A.Pat.var name_var in
             let e_var = A.Exp.ident (lid_of_str name_var) in
             let decoded =
               [%expr
                 let v =
                   [%e
                     decode_expr_of_ty ~ty:ty_i [%expr Array_cursor.current c]]
                 in
                 Array_cursor.consume c;
                 v]
             in
             e_var, A.Vb.mk p_var decoded)
      |> List.split
    in

    [%expr
      Imandrakit_twine.Decode.(
        let c = array dec [%e e] in
        if Array_cursor.length c <> [%e A.Exp.constant (A.Const.int arity)] then
          fail
            [%e A.Exp.constant (A.Const.string (spf "expected %d-tuple" arity))]
        else
          [%e A.Exp.let_ Nonrecursive deser_args @@ A.Exp.tuple tup_vars])]
  | { ptyp_desc = Ptyp_alias (ty, _); _ } -> decode_expr_of_ty e ~ty
  | { ptyp_desc = Ptyp_arrow _; ptyp_loc = loc; _ } ->
    [%expr [%error "Cannot deserialize functions"]]
  | { ptyp_desc = Ptyp_class _ | Ptyp_object _; ptyp_loc = loc; _ } ->
    [%expr [%error "Cannot deserialize objects yet"]]
  | { ptyp_desc = Ptyp_package _; ptyp_loc = loc; _ } ->
    [%expr [%error "Cannot deserialize first-class modules"]]
  | { ptyp_desc = Ptyp_extension _; ptyp_loc = loc; _ } ->
    [%expr [%error "Cannot deserialize type extensions"]]
  | { ptyp_desc = Ptyp_variant _; ptyp_loc = loc; _ } ->
    [%expr [%error "Cannot deserialize polymorphic variants yet"]]
  | { ptyp_desc = Ptyp_any; ptyp_loc = loc; _ } ->
    [%expr [%error "Cannot deserialize values of type `_`"]]
  | { ptyp_desc = Ptyp_poly _; ptyp_loc = loc; _ } ->
    [%expr [%error "Cannot deserialize values of this type"]]

(** Code for the serialization function for this type decl *)
let encode_expr_of_tydecl (decl : type_declaration) : expression =
  let loc = decl.ptype_loc in
  let self = A.Exp.ident @@ lid ~loc "self" in

  let body =
    match decl.ptype_kind with
    | Ptype_abstract ->
      (match decl.ptype_manifest with
      | Some ty_alias ->
        immediate_expr_of_ty self ~ty:ty_alias (* alias, just forward to it *)
      | None -> [%expr [%error "cannot derive twine for abstract type"]])
    | Ptype_open -> [%expr [%error "cannot derive twine for open type"]]
    | Ptype_variant cstors ->
      let ser_cstor (index : int)
          { pcd_args; pcd_name = cname; pcd_loc = loc; _ } : case =
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
                       immediate_expr_of_ty ~ty
                         (A.Exp.ident @@ lid ~loc @@ spf "x_%d" i))
                |> A.Exp.array ~loc
              in
              [%expr
                Imandrakit_twine.Encode.cstor enc ~index:[%e e_index] [%e args]]
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
                       immediate_expr_of_ty field ~ty:pld_type)
                |> A.Exp.array ~loc
              in
              [%expr
                Imandrakit_twine.Encode.cstor enc ~index:[%e e_index] [%e args]]
            in
            lhs, rhs
        in

        B.case ~lhs ~guard:None ~rhs
      in
      let branches = List.mapi ser_cstor cstors in
      A.Exp.match_ self branches
    | Ptype_record labels ->
      (* TODO: if some config asks for it, use a pointer-with-metadata
         to pair the tuple with a record descriptor *)
      let fields =
        labels
        |> List.map (fun { pld_name = field_name; pld_type; _ } ->
               let self_field = A.Exp.field self @@ lid_of_str field_name in
               immediate_expr_of_ty self_field ~ty:pld_type)
      in
      [%expr Imandrakit_twine.Encode.array enc [%e A.Exp.array fields]]
  in

  [%expr fun enc self -> [%e body]]

let decode_expr_of_tydecl (decl : type_declaration) : expression =
  let loc = decl.ptype_loc in
  let self = A.Exp.ident @@ lid ~loc "self" in

  let body =
    match decl.ptype_kind with
    | Ptype_abstract ->
      (match decl.ptype_manifest with
      | Some ty_alias ->
        decode_expr_of_ty self ~ty:ty_alias (* alias, just forward to it *)
      | None -> [%expr [%error "cannot derive twine for abstract type"]])
    | Ptype_open -> [%expr [%error "cannot derive twine for open type"]]
    | Ptype_variant cstors ->
      let dec_cstor (index : int)
          { pcd_args; pcd_name = cname; pcd_loc = loc; _ } : case =
        let p_index = A.Pat.constant (A.Const.int index) in

        let read_cstor_args (tys : core_type list) :
            expression list * value_binding list =
          tys
          |> List.mapi (fun i ty ->
                 let loc = ty.ptyp_loc in
                 let name = { loc; txt = spf "x_%d" i } in
                 let p_var = A.Pat.var name in
                 let e_var = A.Exp.ident (lid_of_str name) in
                 let rhs =
                   [%expr
                     let v =
                       [%e
                         decode_expr_of_ty ~ty
                           [%expr
                             Imandrakit_twine.Decode.Array_cursor.current args]]
                     in
                     Imandrakit_twine.Decode.Array_cursor.consume args;
                     v]
                 in
                 e_var, A.Vb.mk p_var rhs)
          |> List.split
        in

        let lhs, rhs =
          match pcd_args with
          | Pcstr_tuple [] ->
            let lhs = [%pat? Imandrakit_twine.Decode.Value.Cstor0 [%p p_index]]
            and rhs = A.Exp.construct (lid_of_str cname) None in
            lhs, rhs
          | Pcstr_tuple [ ty0 ] ->
            let lhs =
              [%pat? Imandrakit_twine.Decode.Value.Cstor1 ([%p p_index], p)]
            in
            let rhs =
              [%expr
                let x = [%e decode_expr_of_ty [%expr p] ~ty:ty0] in
                [%e A.Exp.construct (lid_of_str cname) (Some [%expr x])]]
            in
            lhs, rhs
          | Pcstr_tuple l ->
            let lhs =
              [%pat? Imandrakit_twine.Decode.Value.CstorN ([%p p_index], args)]
            in
            let vars, read_args = read_cstor_args l in
            let rhs =
              let args = A.Exp.tuple vars in
              A.Exp.(
                let_ Nonrecursive read_args
                @@ construct (lid_of_str cname) (Some args))
            in
            lhs, rhs
          | Pcstr_record r ->
            let lhs =
              [%pat? Imandrakit_twine.Decode.Value.CstorN ([%p p_index], args)]
            in
            let rhs =
              let vars, read_args =
                read_cstor_args (List.map (fun { pld_type = ty; _ } -> ty) r)
              in
              let fields =
                List.map2
                  (fun { pld_name = field_name; _ } var ->
                    lid_of_str field_name, var)
                  r vars
              in
              let r = A.Exp.record fields None in
              A.Exp.(
                let_ Nonrecursive read_args
                @@ construct (lid_of_str cname) (Some r))
            in
            lhs, rhs
        in
        B.case ~lhs ~guard:None ~rhs
      in
      let fail =
        let lhs = [%pat? _]
        and rhs =
          let err =
            A.Exp.constant
              (A.Const.string
              @@ spf "expected value of type %s" decl.ptype_name.txt)
          in
          [%expr Imandrakit_twine.Decode.fail [%e err]]
        in
        B.case ~lhs ~rhs ~guard:None
      in
      let branches = List.mapi dec_cstor cstors @ [ fail ] in
      [%expr
        let e = Imandrakit_twine.Decode.(read dec @@ deref_rec dec [%e self]) in
        [%e A.Exp.match_ [%expr e] branches]]
    | Ptype_record labels ->
      let fields, vbs =
        labels
        |> List.map (fun { pld_name = field_name; pld_type; _ } ->
               let field_of_self =
                 [%expr
                   let v =
                     [%e
                       decode_expr_of_ty ~ty:pld_type
                         [%expr Imandrakit_twine.Decode.Array_cursor.current c]]
                   in
                   Imandrakit_twine.Decode.Array_cursor.consume c;
                   v]
               in
               let vb = A.Vb.mk (A.Pat.var field_name) field_of_self in
               (lid_of_str field_name, A.Exp.ident (lid_of_str field_name)), vb)
        |> List.split
      in
      [%expr
        let c = Imandrakit_twine.Decode.array dec self in
        [%e A.Exp.let_ Nonrecursive vbs @@ A.Exp.record ~loc fields None]]
  in
  [%expr fun dec self -> [%e body]]

exception Error_gen of Location.t * string

let error_gen ~loc e = raise (Error_gen (loc, e))

let param_names ty =
  ty.ptype_params
  |> List.map (fun (ty, _) ->
         let loc = ty.ptyp_loc in
         match ty.ptyp_desc with
         | Ptyp_var a -> a
         | Ptyp_any ->
           error_gen ~loc "Cannot derive cbor pack for implicit param"
         | _ -> error_gen ~loc "Cannot derive cbor pack for non-variable type")

let generate_impl_ (rec_flag, type_declarations) =
  (* parametrize by functions for each type variable *)
  let fun_poly_gen_ ~loc ty body =
    mk_lambda ~loc (List.map name_poly_var_ @@ param_names ty) body
  in

  (* forward declarations *)
  let forward_defs_refs : structure_item list =
    List.concat_map
      (fun ty ->
        if ty.ptype_params <> [] then
          (* no ref for polymorphic types (value restriction gets in the way) *)
          []
        else (
          let loc = ty.ptype_loc in
          let f_encode_name = ser_name_ref_of_ty ty in
          let f_decode_name = deser_name_ref_of_ty ty in
          let def_encode = [%expr ref (fun _ _ -> assert false)] in
          let str_encode =
            A.Str.value Nonrecursive
              [ A.Vb.mk (A.Pat.var { loc; txt = f_encode_name }) def_encode ]
          in
          let def_decode = [%expr ref (fun _ _ -> assert false)] in
          let str_decode =
            A.Str.value Nonrecursive
              [ A.Vb.mk (A.Pat.var { loc; txt = f_decode_name }) def_decode ]
          in
          [ str_encode; str_decode ]
        ))
      type_declarations
  in

  (* [let f enc x = !f_ref enc x] for mono types.
     This must come before the actual definitions because of the
     possibility of mutual recursion. *)
  let defs_forward =
    List.concat_map
      (fun ty ->
        if ty.ptype_params <> [] then
          []
        else (
          let loc = ty.ptype_loc in
          let str_encode =
            let fname = ser_name_of_ty ty in
            let fref_name = ser_name_ref_of_ty ty in

            let body = [%expr ![%e A.Exp.ident @@ lid ~loc fref_name]] in
            let body = [%expr [%e body] enc self] in
            let def = [%expr fun enc self -> [%e body]] in
            A.Str.value Nonrecursive
              [ A.Vb.mk (A.Pat.var { loc; txt = fname }) def ]
          and str_decode =
            let fname = deser_name_of_ty ty in
            let fref_name = deser_name_ref_of_ty ty in

            let body = [%expr ![%e A.Exp.ident @@ lid ~loc fref_name]] in
            let body = [%expr [%e body] enc self] in
            let def = [%expr fun enc self -> [%e body]] in
            A.Str.value Nonrecursive
              [ A.Vb.mk (A.Pat.var { loc; txt = fname }) def ]
          in
          [ str_encode; str_decode ]
        ))
      type_declarations
  in

  let ser_defs_poly =
    List.filter_map
      (fun ty ->
        if ty.ptype_params = [] then
          None
        else (
          let loc = ty.ptype_loc in
          let fname = ser_name_of_ty ty in
          let def = fun_poly_gen_ ~loc ty @@ encode_expr_of_tydecl ty in
          let vb = A.Vb.mk (A.Pat.var { loc; txt = fname }) def in
          Some vb
        ))
      type_declarations
  in
  let ser_defs_poly =
    if ser_defs_poly = [] then
      []
    else
      [ A.Str.value rec_flag ser_defs_poly ]
  in

  let deser_defs_poly =
    List.filter_map
      (fun ty ->
        if ty.ptype_params = [] then
          None
        else (
          let loc = ty.ptype_loc in
          let fname = deser_name_of_ty ty in
          let def = fun_poly_gen_ ~loc ty @@ decode_expr_of_tydecl ty in
          let vb = A.Vb.mk (A.Pat.var { loc; txt = fname }) def in
          Some vb
        ))
      type_declarations
  in
  let deser_defs_poly =
    if deser_defs_poly = [] then
      []
    else
      [ A.Str.value rec_flag deser_defs_poly ]
  in

  (* generate (de)serialization code for mono types *)
  let defs_init =
    List.concat_map
      (fun ty ->
        if ty.ptype_params <> [] then
          []
        else (
          let loc = ty.ptype_loc in
          let str_encode =
            let def = encode_expr_of_tydecl ty in
            A.Str.eval
              [%expr
                [%e A.Exp.ident (lid ~loc @@ ser_name_ref_of_ty ty)] := [%e def]]
          and str_decode =
            let def = decode_expr_of_tydecl ty in
            A.Str.eval
              [%expr
                [%e A.Exp.ident (lid ~loc @@ deser_name_ref_of_ty ty)]
                := [%e def]]
          in
          [ str_encode; str_decode ]
        ))
      type_declarations
  in

  let bracket_warn stri =
    let loc =
      match type_declarations with
      | [] -> Location.none
      | ty :: _ -> ty.ptype_loc
    in
    let disable = [%stri [@@@ocaml.warning "-27-33-39"]] in
    let enable = [%stri [@@@ocaml.warning "+27+33+39"]] in
    (disable :: stri) @ [ enable ]
  in
  bracket_warn
    (List.flatten
       [
         forward_defs_refs;
         defs_forward;
         ser_defs_poly;
         deser_defs_poly;
         defs_init;
       ])

let generate_impl ~ctxt:_ (rec_flag, type_declarations) =
  try generate_impl_ (rec_flag, type_declarations)
  with Error_gen (loc, msg) ->
    (* emit an error in generated code *)
    let str0 =
      [%stri let () = [%error [%e A.Exp.constant (A.Const.string msg)]]]
    in
    [ str0 ]

let generate_intf_ type_declarations =
  let values =
    List.map
      (fun ty ->
        let loc = ty.ptype_loc in
        let poly_sers =
          param_names ty
          |> List.map (fun n ->
                 [%type: [%t A.Typ.var n] Imandrakit_twine.Encode.encoder])
        and poly_desers =
          param_names ty
          |> List.map (fun n ->
                 [%type: [%t A.Typ.var n] Imandrakit_twine.Decode.decoder])
        in

        (* type expression for [ty], like [(int, bool) mypair] *)
        let tye =
          A.Typ.constr ~loc (lid_of_str ty.ptype_name)
            (List.map A.Typ.var @@ param_names ty)
        in

        (* declare top function *)
        let name_ser = ser_name_of_ty ty in
        let name_deser = deser_name_of_ty ty in
        let decl_ser =
          let ty =
            mk_arrow ~loc poly_sers
            @@ [%type: [%t tye] Imandrakit_twine.Encode.encoder]
          in
          A.Val.mk { loc; txt = name_ser } ty
        and decl_deser =
          let ty =
            mk_arrow ~loc poly_desers
            @@ [%type: [%t tye] Imandrakit_twine.Decode.decoder]
          in
          A.Val.mk { loc; txt = name_deser } ty
        in
        [ decl_ser; decl_deser ])
      type_declarations
    |> List.flatten
  in
  List.map (fun s -> A.Sig.value s) values

let generate_intf ~ctxt:_ (_rec_flag, type_declarations) =
  try generate_intf_ type_declarations
  with Error_gen (loc, msg) ->
    (* emit an error in generated code *)
    let s =
      [%sigi: val _bad_ser : [%error [%e A.Exp.constant (A.Const.string msg)]]]
    in
    [ s ]

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let myderiver =
  Deriving.add "twine" ~sig_type_decl:intf_generator
    ~str_type_decl:impl_generator
