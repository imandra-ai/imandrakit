(* we follow
   https://ppxlib.readthedocs.io/en/latest/ppx-for-plugin-authors.html .

   Ported from https://github.com/imandra-ai/cbor-pack/ *)

open Ppxlib
module A = Ast_helper
module B = Ast_builder.Default

let spf = Printf.sprintf

let ser_name_of_ty_name (ty_name : string) : string =
  if ty_name = "t" then
    "to_serpack"
  else
    ty_name ^ "_to_serpack"

let deser_name_of_ty_name (ty_name : string) : string =
  if ty_name = "t" then
    "of_serpack"
  else
    ty_name ^ "_of_serpack"

(* name for variables *)
let name_poly_var_ v = spf "_serpack_poly_%s" v

let rec map_lid ~f (lid : Longident.t) : Longident.t =
  match lid with
  | Longident.Lident name -> Longident.Lident (f name)
  | Longident.Lapply (a, b) -> Longident.Lapply (a, map_lid ~f b)
  | Longident.Ldot (m, a) -> Longident.Ldot (m, f a)

let ser_name_of_lid = map_lid ~f:ser_name_of_ty_name
let deser_name_of_lid = map_lid ~f:deser_name_of_ty_name

let ser_name_of_ty (ty : type_declaration) : string =
  let ty_name = ty.ptype_name.txt in
  ser_name_of_ty_name ty_name

let deser_name_of_ty (ty : type_declaration) : string =
  let ty_name = ty.ptype_name.txt in
  deser_name_of_ty_name ty_name

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
let attr_ser =
  Attribute.declare "serpack.ser" Attribute.Context.core_type
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

(* attribute to force a [string] to encode into CBOR bytes *)
let attr_use_bytes =
  Attribute.declare "serpack.use_bytes" Attribute.Context.core_type
    Ast_pattern.(pstr nil)
    ()

(* field in record: use this key *)
let attr_use_field_name =
  Attribute.declare "serpack.key" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload @@ estring __)
    (fun x -> x)

(* cstor in sum type: use this key *)
let attr_use_cstor_name =
  Attribute.declare "serpack.cstor" Attribute.Context.constructor_declaration
    Ast_pattern.(single_expr_payload @@ estring __)
    (fun x -> x)

(* mutable field in record: placeholder for cyclic value *)
let attr_placeholder =
  Attribute.declare "serpack.placeholder" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

(* on type: do hashcons *)
let attr_hashcons =
  Attribute.declare "serpack.hashcons" Attribute.Context.type_declaration
    Ast_pattern.(pstr nil)
    ()

let attr_deser =
  Attribute.declare "serpack.deser" Attribute.Context.core_type
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

(* apply [Ser.state -> 'a -> cbor] function *)
let apply_full_ser ~loc e_ser e : expression =
  [%expr
    let e = [%e e] in
    ([%e e_ser] ser e : Imandrakit_ser_pack.value)]

(* apply ['a -> cbor] function *)
let apply_basic_ser ~loc e_ser e : expression =
  [%expr
    let e = [%e e] in
    ([%e e_ser] e : Imandrakit_ser_pack.value)]

(* apply [Deser.state -> ptr -> 'a] *)
let apply_full_deser ~loc e_deser p : expression =
  [%expr
    let c : Imandrakit_ser_pack.value = [%e p] in
    [%e e_deser] deser c]

let is_some_ = function
  | Some _ -> true
  | None -> false

(* produce an expression that serializes [e].
   In scope: [ser]. *)
let rec ser_expr_of_ty (e : expression) ~(ty : core_type) : expression =
  let loc = ty.ptyp_loc in
  let by_full_ser ser = apply_full_ser ~loc ser e in
  let by_basic_ser ser = apply_basic_ser ~loc ser e in
  match ty with
  | _ when is_some_ (Attribute.get ~mark_as_seen:false attr_ser ty) ->
    (* custom ser *)
    let e_ser =
      match Attribute.get ~mark_as_seen:true attr_ser ty with
      | None -> assert false
      | Some h -> h
    in
    apply_full_ser ~loc e_ser e
  | [%type: int] -> by_basic_ser [%expr Imandrakit_ser_pack.Ser.int]
  | [%type: int32] -> [%expr Imandrakit_ser_pack.Ser.int (Int32.to_int [%e e])]
  | [%type: int64] ->
    [%expr
      let i = [%e e] in
      Imandrakit_ser_pack.Ser.int64 i]
  | [%type: nativeint] ->
    [%expr
      let i = [%e e] in
      Imandrakit_ser_pack.Ser.int64 (Int64.of_nativeint i)]
  | [%type: string] ->
    if is_some_ @@ Attribute.get ~mark_as_seen:false attr_use_bytes ty then
      [%expr
        Imandrakit_ser_pack.Ser.add_bytes ser (Bytes.unsafe_of_string [%e e])]
    else
      [%expr Imandrakit_ser_pack.Ser.add_string ser [%e e]]
  | [%type: bytes] -> [%expr Imandrakit_ser_pack.Ser.add_bytes ser [%e e]]
  | [%type: bool] -> by_basic_ser [%expr Imandrakit_ser_pack.Ser.bool]
  | [%type: char] ->
    by_basic_ser [%expr fun c -> Imandrakit_ser_pack.Ser.int (Char.code c)]
  | [%type: unit] -> by_basic_ser [%expr fun _ -> Imandrakit_ser_pack.Ser.unit]
  | [%type: float] -> by_basic_ser [%expr Imandrakit_ser_pack.Ser.float]
  | [%type: [%t? ty_arg0] option] ->
    [%expr
      match [%e e] with
      | None -> Imandrakit_ser_pack.Ser.(list [])
      | Some x ->
        let x = [%e ser_expr_of_ty [%expr x] ~ty:ty_arg0] in
        Imandrakit_ser_pack.Ser.(list [ x ])]
  | [%type: [%t? ty_arg0] list] ->
    [%expr
      Imandrakit_ser_pack.Ser.list_of
        (fun ser x -> [%e ser_expr_of_ty [%expr x] ~ty:ty_arg0])
        ser [%e e]]
  | [%type: [%t? ty_arg0] array] ->
    [%expr
      Imandrakit_ser_pack.Ser.list_of
        (fun ser x -> [%e ser_expr_of_ty [%expr x] ~ty:ty_arg0])
        ser
        (Array.to_list [%e e])]
  | { ptyp_desc = Ptyp_var v; ptyp_loc = loc; _ } ->
    (* use function passed as a parameter for each polymorphic argument *)
    let s = A.Exp.ident @@ lid ~loc @@ name_poly_var_ v in
    by_full_ser s
  | { ptyp_desc = Ptyp_constr (lid, args); ptyp_loc = loc; _ } ->
    (* find function for this type and apply it to args, themselves functions *)
    let f = A.Exp.ident { loc; txt = ser_name_of_lid lid.txt } in
    let args =
      args
      |> List.map (fun ty ->
             let ser = [%expr fun ser x -> [%e ser_expr_of_ty [%expr x] ~ty]] in
             Nolabel, ser)
    in
    by_full_ser
      (if args = [] then
        f
      else
        A.Exp.apply f args)
  | { ptyp_desc = Ptyp_tuple args; ptyp_loc = loc; _ } ->
    let ser_args =
      args
      |> List.mapi (fun i ty ->
             let x_i = A.Exp.ident (lid ~loc @@ spf "x_%d" i) in
             ser_expr_of_ty x_i ~ty)
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
      [%expr Imandrakit_ser_pack.Ser.(list [%e mk_list ~loc ser_args])]
    in
    A.Exp.let_ Nonrecursive vbs body
  | { ptyp_desc = Ptyp_variant _; ptyp_loc = loc; _ } ->
    (* TODO *)
    [%expr [%error "Cannot serialize polymorphic variants yet"]]
  | { ptyp_desc = Ptyp_alias (ty, _); _ } -> ser_expr_of_ty e ~ty
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

(* produce an expression that serializes [e].
   In scope: [deser]. *)
let rec deser_expr_of_ty (e : expression) ~(ty : core_type) : expression =
  let loc = ty.ptyp_loc in
  let by_full_deser ser = apply_full_deser ~loc ser e in
  match ty with
  | _ when is_some_ (Attribute.get ~mark_as_seen:false attr_deser ty) ->
    (* custom deser *)
    let e_deser =
      match Attribute.get ~mark_as_seen:true attr_deser ty with
      | None -> assert false
      | Some h -> h
    in
    apply_full_deser ~loc e_deser e
  | [%type: int] -> by_full_deser [%expr Imandrakit_ser_pack.Deser.to_int]
  | [%type: int32] ->
    [%expr Imandrakit_ser_pack.Deser.to_int deser [%e e] |> Int32.of_int]
  | [%type: int64] ->
    [%expr
      match Imandrakit_ser_pack.Deser.deref_if_ptr deser [%e e] with
      | Int i -> i
      | Str s ->
        (try Int64.of_string s
         with _ -> Imandrakit_ser_pack.Deser.fail "expected int64")
      | _ -> Imandrakit_ser_pack.Deser.fail "expected int64"]
  | [%type: nativeint] ->
    [%expr
      match Imandrakit_ser_pack.Deser.deref_if_ptr deser [%e e] with
      | Int i -> Int64.to_nativeint i
      | Str s ->
        (try Nativeint.of_string s
         with _ -> Imandrakit_ser_pack.Deser.fail "expected nativeint")
      | _ -> Imandrakit_ser_pack.Deser.fail "expected nativeint"]
  | [%type: string] ->
    if is_some_ @@ Attribute.get ~mark_as_seen:false attr_use_bytes ty then
      [%expr
        Bytes.unsafe_to_string
        @@ Imandrakit_ser_pack.Deser.to_bytes deser
        @@ [%e e]]
    else
      by_full_deser [%expr Imandrakit_ser_pack.Deser.to_text]
  | [%type: bytes] -> by_full_deser [%expr Imandrakit_ser_pack.Deser.to_bytes]
  | [%type: bool] -> by_full_deser [%expr Imandrakit_ser_pack.Deser.to_bool]
  | [%type: char] ->
    [%expr Char.chr (Imandrakit_ser_pack.Deser.to_int deser [%e e])]
  | [%type: unit] -> by_full_deser [%expr Imandrakit_ser_pack.Deser.to_unit]
  | [%type: float] -> by_full_deser [%expr Imandrakit_ser_pack.Deser.to_float]
  | [%type: [%t? ty_arg0] option] ->
    [%expr
      match Imandrakit_ser_pack.Deser.(to_list deser [%e e]) with
      | [] -> None
      | [ x ] ->
        let x = [%e deser_expr_of_ty [%expr x] ~ty:ty_arg0] in
        Some x
      | _ ->
        Imandrakit_ser_pack.Deser.fail "expected an option (list of 0 or 1 elt)"]
  | [%type: [%t? ty_arg0] list] ->
    [%expr
      let l = Imandrakit_ser_pack.Deser.(to_list deser [%e e]) in
      List.map (fun x -> [%e deser_expr_of_ty [%expr x] ~ty:ty_arg0]) l]
  | [%type: [%t? ty_arg0] array] ->
    [%expr
      let a = Array.of_list @@ Imandrakit_ser_pack.Deser.to_list deser [%e e] in
      Array.map (fun x -> [%e deser_expr_of_ty [%expr x] ~ty:ty_arg0]) a]
  | { ptyp_desc = Ptyp_var v; ptyp_loc = loc; _ } ->
    (* use function passed as a parameter for each polymorphic argument *)
    let s = A.Exp.ident @@ lid ~loc @@ name_poly_var_ v in
    by_full_deser s
  | { ptyp_desc = Ptyp_constr (lid, args); ptyp_loc = loc; _ } ->
    (* find function for this type and apply it to args, themselves functions *)
    let f = A.Exp.ident { loc; txt = deser_name_of_lid lid.txt } in
    let args =
      args
      |> List.map (fun ty ->
             let f =
               [%expr fun deser x -> [%e deser_expr_of_ty [%expr x] ~ty]]
             in
             Nolabel, f)
    in
    by_full_deser
      (if args = [] then
        f
      else
        A.Exp.apply f args)
  | { ptyp_desc = Ptyp_tuple args; ptyp_loc = loc; _ } ->
    let deser_args =
      args
      |> List.mapi (fun i ty_i ->
             let x_i = A.Exp.ident (lid ~loc @@ spf "x_%d" i) in
             deser_expr_of_ty x_i ~ty:ty_i)
    in
    (* [match e with [x1,...,xn] -> … | _ -> fail] *)
    let br_good =
      let lhs =
        mk_list_pat ~loc
        @@ List.mapi
             (fun i ty_i ->
               let loc = ty_i.ptyp_loc in
               A.Pat.var { loc; txt = spf "x_%d" i })
             args
      and rhs = A.Exp.tuple ~loc deser_args in
      A.Exp.case [%pat? List [%p lhs] as _p] rhs
    and br_bad1 =
      let msg =
        A.Const.string
          (spf "wrong tuple arity (expected %d)" (List.length args))
      in
      A.Exp.case
        [%pat? List _]
        [%expr Imandrakit_ser_pack.Deser.fail [%e A.Exp.constant msg]]
    and br_bad2 =
      A.Exp.case
        [%pat? _]
        [%expr Imandrakit_ser_pack.Deser.fail "expected tuple"]
    in
    A.Exp.match_ ~loc
      [%expr Imandrakit_ser_pack.Deser.deref_if_ptr deser [%e e]]
      [ br_good; br_bad1; br_bad2 ]
  | { ptyp_desc = Ptyp_alias (ty, _); _ } -> deser_expr_of_ty e ~ty
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

let ser_expr_of_tydecl (decl : type_declaration) : expression =
  let loc = decl.ptype_loc in
  let self = A.Exp.ident @@ lid ~loc "self" in

  let body =
    match decl.ptype_kind with
    | Ptype_abstract ->
      (match decl.ptype_manifest with
      | Some ty_alias ->
        ser_expr_of_ty self ~ty:ty_alias (* alias, just forward to it *)
      | None -> [%expr [%error "cannot derive serpack for abstract type"]])
    | Ptype_open -> [%expr [%error "cannot derive serpack for open type"]]
    | Ptype_variant cstors ->
      (* is it an enum? (no payload in any cstor) *)
      let ser_cstor ({ pcd_args; pcd_name = cname; pcd_loc = loc; _ } as cstor)
          : case =
        (* constructor identifier *)
        let cstor_key =
          match Attribute.get ~mark_as_seen:true attr_use_cstor_name cstor with
          | None ->
            [%expr
              Imandrakit_ser_pack.Ser.string
                [%e A.Exp.constant (A.Const.string cname.txt)]]
          | Some s ->
            [%expr
              Imandrakit_ser_pack.Ser.string
                [%e A.Exp.constant (A.Const.string s)]]
        in

        let lhs, rhs =
          match pcd_args with
          | Pcstr_tuple [] ->
            let lhs = A.Pat.construct (lid_of_str cname) None
            and rhs = cstor_key in
            lhs, rhs
          | Pcstr_tuple [ ty0 ] ->
            let lhs =
              let x0 = A.Pat.var { loc; txt = "x" } in
              A.Pat.construct (lid_of_str cname) (Some x0)
            in
            let rhs =
              let x0 = A.Exp.ident @@ lid ~loc "x" in
              [%expr
                Imandrakit_ser_pack.Ser.list
                  [ [%e cstor_key]; [%e ser_expr_of_ty x0 ~ty:ty0] ]]
            in
            lhs, rhs
          | Pcstr_tuple l ->
            let lhs =
              let pat =
                l
                |> List.mapi (fun i ty ->
                       let loc = ty.ptyp_loc in
                       A.Pat.var { loc; txt = spf "x_%d" i })
                |> A.Pat.tuple
              in
              A.Pat.construct (lid_of_str cname) (Some pat)
            in
            let rhs =
              let ser_fields =
                l
                |> List.mapi (fun i ty ->
                       let loc = ty.ptyp_loc in
                       ser_expr_of_ty ~ty
                         (A.Exp.ident @@ lid ~loc @@ spf "x_%d" i))
                |> mk_list ~loc
              in
              [%expr
                Imandrakit_ser_pack.Ser.list ([%e cstor_key] :: [%e ser_fields])]
            in

            lhs, rhs
          | Pcstr_record r ->
            (* variable for the record *)
            let pat_r = A.Pat.var { loc; txt = "r" } in
            let lhs = A.Pat.construct (lid_of_str cname) (Some pat_r) in
            let rhs =
              (* variable for the inline record *)
              let var_r = A.Exp.ident (lid ~loc "r") in
              let ser_fields =
                r
                |> List.map (fun { pld_name; pld_loc = _; pld_type; _ } ->
                       let field = A.Exp.field var_r (lid_of_str pld_name) in
                       ser_expr_of_ty field ~ty:pld_type)
                |> mk_list ~loc
              in
              [%expr
                Imandrakit_ser_pack.Ser.list ([%e cstor_key] :: [%e ser_fields])]
            in
            lhs, rhs
        in

        B.case ~lhs ~guard:None ~rhs
      in
      let branches = List.map ser_cstor cstors in
      A.Exp.match_ self branches
    | Ptype_record labels ->
      (* make a map *)
      let e =
        labels
        |> List.map (fun ({ pld_name = field_name; pld_type; _ } as fld) ->
               let self_field = A.Exp.field self @@ lid_of_str field_name in
               let e = ser_expr_of_ty self_field ~ty:pld_type in
               let key =
                 match
                   Attribute.get ~mark_as_seen:true attr_use_field_name fld
                 with
                 | Some k -> [%expr [%e A.Exp.constant (A.Const.string k)]]
                 | None ->
                   [%expr [%e A.Exp.constant (A.Const.string field_name.txt)]]
               in
               [%expr [%e key], [%e e]])
        |> mk_list ~loc
      in
      [%expr Imandrakit_ser_pack.Ser.map [%e e]]
  in

  [%expr
    fun ser self ->
      let open Imandrakit_ser_pack.Ser in
      [%e
        if is_some_ (Attribute.get ~mark_as_seen:true attr_hashcons decl) then
          [%expr add_entry_hashcons ser @@ [%e body]]
        else
          body]]

let deser_expr_of_tydecl (decl : type_declaration) : expression =
  let loc = decl.ptype_loc in
  let self = A.Exp.ident @@ lid ~loc "self" in

  let body =
    match decl.ptype_kind with
    | Ptype_abstract ->
      (match decl.ptype_manifest with
      | Some ty_alias ->
        deser_expr_of_ty self ~ty:ty_alias (* alias, just forward to it *)
      | None -> [%expr [%error "cannot derive serpack for abstract type"]])
    | Ptype_open -> [%expr [%error "cannot derive serpack for open type"]]
    | Ptype_variant cstors ->
      let deser_cstor
          ({ pcd_args; pcd_name = cname; pcd_loc = loc; _ } as cstor) : case =
        (* constructor identifier *)
        let cstor_key =
          match Attribute.get ~mark_as_seen:true attr_use_cstor_name cstor with
          | None -> [%pat? Str [%p A.Pat.constant (A.Const.string cname.txt)]]
          | Some s -> [%pat? Str [%p A.Pat.constant (A.Const.string s)]]
        in

        let lhs, rhs =
          match pcd_args with
          | Pcstr_tuple [] ->
            let lhs = cstor_key
            and rhs = A.Exp.construct (lid_of_str cname) None in
            lhs, rhs
          | Pcstr_tuple [ ty0 ] ->
            let lhs = [%pat? List [ [%p cstor_key]; p ]] in
            let rhs =
              [%expr
                let x = [%e deser_expr_of_ty [%expr p] ~ty:ty0] in
                [%e A.Exp.construct (lid_of_str cname) (Some [%expr x])]]
            in
            lhs, rhs
          | Pcstr_tuple l ->
            let lhs =
              let args =
                l
                |> List.mapi (fun i ty ->
                       let loc = ty.ptyp_loc in
                       A.Pat.var { loc; txt = spf "x_%d" i })
              in
              [%pat? List [%p mk_list_pat ~loc (cstor_key :: args)]]
            in
            let rhs =
              let deser_fields =
                l
                |> List.mapi (fun i ty_i ->
                       let loc = ty_i.ptyp_loc in
                       deser_expr_of_ty ~ty:ty_i
                         (A.Exp.ident @@ lid ~loc @@ spf "x_%d" i))
                |> A.Exp.tuple
              in
              A.Exp.construct (lid_of_str cname) (Some deser_fields)
            in
            lhs, rhs
          | Pcstr_record r ->
            let lhs =
              let args =
                r
                |> List.mapi (fun i { pld_name = _; pld_type = ty; _ } ->
                       let loc = ty.ptyp_loc in
                       A.Pat.var { loc; txt = spf "x_%d" i })
              in
              [%pat? List [%p mk_list_pat ~loc (cstor_key :: args)]]
            in
            let rhs =
              let fields =
                r
                |> List.mapi
                     (fun
                       i
                       {
                         pld_name = field_name;
                         pld_loc = loc;
                         pld_type = ty_r;
                         _;
                       }
                     ->
                       let var_r = A.Exp.ident @@ lid ~loc @@ spf "x_%d" i in
                       lid_of_str field_name, deser_expr_of_ty var_r ~ty:ty_r)
              in
              let r = A.Exp.record fields None in
              A.Exp.construct (lid_of_str cname) (Some r)
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
          [%expr Imandrakit_ser_pack.Deser.fail [%e err]]
        in
        B.case ~lhs ~rhs ~guard:None
      in
      let branches = List.map deser_cstor cstors @ [ fail ] in
      [%expr
        let e = Imandrakit_ser_pack.Deser.deref_if_ptr deser [%e self] in
        [%e A.Exp.match_ [%expr e] branches]]
    | Ptype_record labels ->
      let fields =
        labels
        |> List.map (fun ({ pld_name = field_name; pld_type; _ } as fld) ->
               let key =
                 match
                   Attribute.get ~mark_as_seen:true attr_use_field_name fld
                 with
                 | Some k -> [%expr [%e A.Exp.constant (A.Const.string k)]]
                 | None ->
                   [%expr [%e A.Exp.constant (A.Const.string field_name.txt)]]
               in

               let field_of_self =
                 [%expr
                   Imandrakit_ser_pack.Deser.map_entry deser ~k:[%e key] self]
               in
               let value = deser_expr_of_ty field_of_self ~ty:pld_type in
               lid_of_str field_name, value)
      in
      let res = A.Exp.record ~loc fields None in
      res
  in
  [%expr fun deser self -> [%e body]]

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

  (* generate serialization code *)
  let ser_decls =
    List.map
      (fun ty ->
        let loc = ty.ptype_loc in
        let fname = ser_name_of_ty ty in
        let def = fun_poly_gen_ ~loc ty @@ ser_expr_of_tydecl ty in
        A.Vb.mk (A.Pat.var { loc; txt = fname }) def)
      type_declarations
  in
  let ser_defs = A.Str.value rec_flag ser_decls in

  (* generate deserialization code *)
  let deser_decls =
    List.map
      (fun ty ->
        let loc = ty.ptype_loc in
        let fname = deser_name_of_ty ty in
        let def = fun_poly_gen_ ~loc ty @@ deser_expr_of_tydecl ty in
        A.Vb.mk (A.Pat.var { loc; txt = fname }) def)
      type_declarations
  in
  let deser_defs = A.Str.value rec_flag deser_decls in

  (* deserialization code might be lazy (cyclic values), so here we wrap
     taht in a forcing layer *)
  let deser_defs_wrappers = [] in

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
    (List.flatten [ [ ser_defs ]; [ deser_defs ]; deser_defs_wrappers ])

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
                 [%type: [%t A.Typ.var n] Imandrakit_ser_pack.Ser.t])
        and poly_desers =
          param_names ty
          |> List.map (fun n ->
                 [%type: [%t A.Typ.var n] Imandrakit_ser_pack.Deser.t])
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
            @@ [%type: [%t tye] Imandrakit_ser_pack.Ser.t]
          in
          A.Val.mk { loc; txt = name_ser } ty
        and decl_deser =
          let ty =
            mk_arrow ~loc poly_desers
            @@ [%type: [%t tye] Imandrakit_ser_pack.Deser.t]
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

let impl_generator =
  (* AAAAAAAAAAAHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
     let args = Deriving.Args.(
         let rec_name = arg_option "use_field_names" Ast_pattern.(e alt (bool true) (bool false)) in
         empty +> rec_name
       ) in
     Deriving.Generator.V2.make args generate_impl
  *)
  Deriving.Generator.V2.make_noarg generate_impl

let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let myderiver =
  Deriving.add "serpack" ~sig_type_decl:intf_generator
    ~str_type_decl:impl_generator
