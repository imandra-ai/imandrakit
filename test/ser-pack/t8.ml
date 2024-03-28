module CP = Imandrakit_ser_pack

type foo = {
  x: int;
  y: (string[@use_bytes]) option;
}
[@@deriving serpack]

module Foo = struct
  type t = foo

  let equal = ( = )
  let hash = Hashtbl.hash

  let pp out self =
    Format.fprintf out "{x=%d, y=" self.x;
    match self.y with
    | None -> Format.fprintf out "None}"
    | Some s -> Format.fprintf out "Some %S}" s
end

let key_ser_foo : foo CP.Ser.cache_key = CP.Ser.create_cache_key (module Foo)
let key_deser_foo : foo CP.Deser.cache_key = CP.Deser.create_cache_key ()

type tree =
  | N of foo
  | T of tree_ref * tree_ref

and tree_ref = {
  id: int;
  tree: tree;
}

let rec pp_tree out = function
  | N foo -> Format.fprintf out "N %a" Foo.pp foo
  | T (t1, t2) ->
    Format.fprintf out "T (@[%a,@ %a@])" pp_tree_ref t1 pp_tree_ref t2

and pp_tree_ref out r = Format.fprintf out "%d:=%a" r.id pp_tree r.tree

let mkfoo ?y x : foo = { x; y }
let n foo : tree = N foo
let t r1 r2 : tree = T (r1, r2)

let t_ref =
  let id_ = ref 0 in
  fun t ->
    let id = !id_ in
    incr id_;
    { tree = t; id }

module Tree_ref = struct
  type t = tree_ref

  let equal a b = a.id = b.id
  let hash a = Hashtbl.hash a.id
end

let key_ser_tree_ref : tree_ref CP.Ser.cache_key =
  CP.Ser.create_cache_key (module Tree_ref)

let key_deser_tree_ref : tree_ref CP.Deser.cache_key =
  CP.Deser.create_cache_key ()

let rec ser_tree st (t : tree) : CP.value =
  match t with
  | N foo ->
    CP.Ser.(
      list [ string "N"; CP.Ser.with_cache key_ser_foo foo_to_serpack st foo ])
  | T (r1, r2) ->
    CP.Ser.(list [ string "T"; ser_tree_ref st r1; ser_tree_ref st r2 ])

and ser_tree_ref st (r : tree_ref) : CP.value =
  let ser_uncached st r = CP.Ser.(list [ int r.id; ser_tree st r.tree ]) in
  CP.Ser.with_cache key_ser_tree_ref ser_uncached st r

let rec deser_tree st (c : CP.value) : tree =
  let l = CP.Deser.to_list st c in
  match l with
  | [ Str "N"; foo ] ->
    N (CP.Deser.with_cache key_deser_foo foo_of_serpack st foo)
  | [ Str "T"; t1; t2 ] ->
    let t1 = deser_tree_ref st t1 in
    let t2 = deser_tree_ref st t2 in
    t t1 t2
  | _ -> CP.Deser.fail "expected a tree"

and deser_tree_ref st c : tree_ref =
  let deser_uncached st c =
    match CP.Deser.to_list st c with
    | [ Int id; t ] -> { id = Int64.to_int id; tree = deser_tree st t }
    | _ -> CP.Deser.fail "expected a tree_ref"
  in

  CP.Deser.with_cache key_deser_tree_ref deser_uncached st c

let big_tree =
  let f1 = mkfoo 41 in
  let f2 = mkfoo ~y:"hello" 2 in
  let f3 = mkfoo ~y:"world" 3 in
  let t1 = t_ref @@ n f1 in
  let t2 = t_ref @@ n f2 in
  let t3 = t_ref @@ n f3 in
  let t4 = t_ref (t t1 t2) in
  let t5 = t_ref (t t4 t4) in
  let t6 = t_ref (t t5 t3) in
  let t7 = t_ref (t t5 t6) in
  t_ref (t t7 t7)

let () = Format.printf "big_tree: %a@." pp_tree_ref big_tree
let cbor_big_tree = CP.to_value ser_tree_ref big_tree
let () = Format.printf "ser(big_tree): %a@." CP.pp_diagnostic cbor_big_tree

let () =
  Format.printf "size: %d@."
    (Imandrakit_ser_cbor.encode cbor_big_tree |> String.length)

let () =
  Format.printf "marshal size: %d@."
    (Marshal.to_string big_tree [] |> String.length)

let big_tree2 = CP.of_value_exn deser_tree_ref cbor_big_tree
let () = Format.printf "deser(ser(big_tree)): %a@." pp_tree_ref big_tree2
let () = Format.printf "equal? %B@." (big_tree = big_tree2)
let () = assert (big_tree = big_tree2)
