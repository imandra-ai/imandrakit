type x1 =
  | A of int
  | B
  | C of {
      x: int option;
      z: (bool * char) list;
    }
  | D of x1 * x1
[@@deriving twine, show { with_path = false }] [@@hashcons]

type x1_alias = x1 [@@deriving twine, show]

type x2 = {
  a: x1_alias;
  b: x1 list list array;
  c: x2 list option;
}
[@@deriving twine, show { with_path = false }]
[@@twine.use_field_names]
[@@hashcons]

let c0 = C { x = Some 3; z = [ true, 'a'; false, 'c' ] }

let myx2 =
  let c0 = C { x = Some 3; z = [ true, 'a'; false, 'c' ] } in
  let c1 = B in
  {
    a = A 42;
    b = [| [ []; [ c0 ] ]; [ [ B; c1; c1 ]; [ c0 ]; [ c0; D (c0, c0) ] ] |];
    c = None;
  }

let myx2_big = { a = B; b = [| [ []; [ c0; c0 ] ] |]; c = Some [ myx2; myx2 ] }
;;

Format.printf "start with myx2:@.%a@." pp_x2 myx2

let s = Imandrakit_twine.Encode.to_string x2_to_twine myx2
let () = Printf.printf "serialized to %S\n" s
let () = Printf.printf "len: %d\n" (String.length s)
let s' = Marshal.to_string myx2 [ Marshal.No_sharing ]
let () = Printf.printf "len with marshal: %d\n" (String.length s')
let () = Format.printf "dump:@.%s@." (Imandrakit_twine.Dump.dump_string s)
let foo2 = Imandrakit_twine.Decode.decode_string x2_of_twine s
let () = Format.printf "myx2 after roundtrip:@.%a@." pp_x2 foo2;;

assert (foo2 = myx2)

let s_big = Imandrakit_twine.Encode.to_string x2_to_twine myx2_big
let () = Printf.printf "len: %d\n" (String.length s_big)
let s'_big = Marshal.to_string myx2_big [ Marshal.No_sharing ]
let () = Printf.printf "len(big) with marshal: %d\n" (String.length s'_big)
let foo2_big = Imandrakit_twine.Decode.decode_string x2_of_twine s_big
let () = Format.printf "deser(myx2_big):@.%a@." pp_x2 foo2_big

let () =
  Format.printf "myx2_big hex is:@.%s@." (Hex.hexdump_s @@ Hex.of_string s_big)
;;

assert (myx2_big = foo2_big)
