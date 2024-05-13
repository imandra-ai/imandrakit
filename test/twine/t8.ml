type foo = {
  x: int;
  y: (string[@use_bytes]) option;
}
[@@deriving twine]

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

let () =
  Imandrakit_twine.Encode.add_cache (module Foo) foo_to_twine_ref;
  Imandrakit_twine.Decode.add_cache foo_of_twine_ref

type many_foos = { foos: foo list } [@@deriving twine]

let mkfoo ?y x : foo = { x; y }
let foos = { foos = List.init 50 (fun _ -> mkfoo ~y:"yolo" 9999) }
let s_foos = Imandrakit_twine.Encode.to_string many_foos_to_twine foos

let () =
  Format.printf "many foos as str: %d bytes@.%s@." (String.length s_foos)
    (Hex.hexdump_s @@ Hex.of_string s_foos)

let s_foos_m = Marshal.to_string foos []

let () =
  Format.printf "many foos marshalled: %d bytes@.%s@." (String.length s_foos_m)
    (Hex.hexdump_s @@ Hex.of_string s_foos_m)

let () =
  Format.printf "dump of many foos:@.%s@."
    (Imandrakit_twine.Dump.dump_string s_foos)

(** {2 Trees} *)

type tree =
  | N of foo
  | T of tree_ref * tree_ref

and tree_ref = {
  id: int;
  tree: tree;
}
[@@deriving twine]

let rec pp_tree out = function
  | N foo -> Format.fprintf out "N %a" Foo.pp foo
  | T (t1, t2) ->
    Format.fprintf out "T (@[%a,@ %a@])" pp_tree_ref t1 pp_tree_ref t2

and pp_tree_ref out r = Format.fprintf out "%d:=%a" r.id pp_tree r.tree

let n foo : tree = N foo
let t r1 r2 : tree = T (r1, r2)

let t_ref : tree -> tree_ref =
  let id_ = ref 0 in
  fun t ->
    let id = !id_ in
    incr id_;
    { tree = t; id }

module Tree_ref = struct
  type t = tree_ref [@@deriving twine]

  let equal a b = a.id = b.id
  let hash a = Hashtbl.hash a.id
end

let () =
  Imandrakit_twine.Encode.add_cache (module Tree_ref) tree_ref_to_twine_ref;
  Imandrakit_twine.Decode.add_cache tree_ref_of_twine_ref;
  ()

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
let str_big_tree = Imandrakit_twine.Encode.to_string tree_ref_to_twine big_tree

let () =
  Format.printf "ser(big_tree):@.%s@."
    (Hex.hexdump_s @@ Hex.of_string str_big_tree)

let () = Format.printf "size: %d@." (String.length str_big_tree)

let () =
  let sm = Marshal.to_string big_tree [] in
  Format.printf "marshal size: %d@.%s@." (String.length sm)
    (Hex.hexdump_s @@ Hex.of_string sm)

let () =
  Format.printf "dump of big tree:@.%s@."
    (Imandrakit_twine.Dump.dump_string str_big_tree)

let big_tree2 =
  Imandrakit_twine.Decode.decode_string tree_ref_of_twine str_big_tree

let () = Format.printf "deser(ser(big_tree)): %a@." pp_tree_ref big_tree2
let () = Format.printf "equal? %B@." (big_tree = big_tree2)
let () = assert (big_tree = big_tree2)
