module H = Imandrakit_mhash
module Fnv = Imandrakit_mhash_fnv

type foo0 = { x0: int }

let pf = Printf.printf
let hasher_foo0 = { H.hash_into = (fun hash ctx foo -> hash.H.int ctx foo.x0) }

let () =
  let h1 = H.hash ~algo:Fnv.int ~hash:hasher_foo0 { x0 = 1 } in
  let h2 = H.hash ~algo:Fnv.int ~hash:hasher_foo0 { x0 = 2 } in
  pf "foo0: h1=%d\n" h1;
  pf "foo0: h2=%d\n" h1;
  assert (h1 <> h2);
  ()

type foo1 =
  | A
  | B
[@@deriving mhash]

let () =
  let h1 = H.hash ~algo:Fnv.int ~hash:hasher_foo1 A in
  let h2 = H.hash ~algo:Fnv.int ~hash:hasher_foo1 B in
  pf "foo1: h1=%d\n" h1;
  pf "foo1: h2=%d\n" h1;
  assert (h1 <> h2);
  ()

let () =
  let h1 = hash_foo1 ~algo:Fnv.int A in
  let h2 = hash_foo1 ~algo:Fnv.int B in
  assert (h1 <> h2);
  ()

module Foo : sig
  type t [@@deriving mhash]

  val x1 : t
  val x2 : t
end = struct
  type t = {
    x: bool * string;
    y: bool option;
    z: nativeint * int32 * int64 * char;
  }
  [@@deriving mhash]

  let x1 = { x = true, "x"; y = None; z = 1n, 1l, 1L, '1' }
  let x2 = { x = false, "y"; y = Some true; z = 2n, 2l, 2L, '2' }
end

let () =
  let h1 = Foo.hash ~algo:Fnv.int Foo.x1 in
  let h2 = Foo.hash ~algo:Fnv.int Foo.x2 in
  pf "Foo.t: h1=%d\n" h1;
  pf "Foo.t: h2=%d\n" h2;
  assert (h1 <> h2);
  ()

type big_sum =
  | B1
  | B2 of int
  | B3 of int * bool
  | B4 of {
      x: int;
      y: string;
      z: bool option;
    }
[@@deriving mhash]

let () =
  pf "h b1=%d\n" (hash_big_sum ~algo:Fnv.int B1);
  pf "h b2=%d\n" (hash_big_sum ~algo:Fnv.int @@ B2 42);
  pf "h b3=%d\n" (hash_big_sum ~algo:Fnv.int @@ B3 (41, true));
  pf "h b4=%d\n"
    (hash_big_sum ~algo:Fnv.int @@ B4 { x = 1; y = "y"; z = Some false });
  ()

module Not_hashable : sig
  type t = private string

  val x1 : t
  val x2 : t
end = struct
  type t = string

  let x1 = "ohno"
  let x2 = "wat"
end

type foos = {
  all_foos: Foo.t list list;
  any_foo: Foo.t option;
  foos_ignore: (Not_hashable.t[@nohash]);
  big_data_foo: Foo.t array;
}
[@@deriving mhash]

let () =
  let x =
    {
      all_foos = [ [ Foo.x1 ]; [ Foo.x2 ] ];
      any_foo = None;
      foos_ignore = Not_hashable.x1;
      big_data_foo = [||];
    }
  in
  let x' = { x with foos_ignore = Not_hashable.x2 } in
  pf "hash foos 1=%d\n" (Fnv.hash_int hasher_foos x);
  pf "hash foos 2 (same)=%d\n" (Fnv.hash_int hasher_foos x')

module Poly : sig
  type ('a, 'b) poly [@@deriving mhash]

  val mk : 'a -> 'b -> ('a, 'b) poly
end = struct
  type ('a, 'b) poly = {
    p_list: 'a list;
    p_tup: ('a * 'b * ('b[@hasher H.trivial]) option) option;
  }
  [@@deriving mhash]

  let mk x y = { p_list = [ x ]; p_tup = Some (x, y, None) }
end

let () =
  pf "hash poly=%d\n"
    (Fnv.hash_int (Poly.hasher_poly H.int H.string) (Poly.mk 1 "yolo"))

let () = print_endline "OK"
