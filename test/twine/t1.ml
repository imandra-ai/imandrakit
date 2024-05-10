type foo = {
  a: int;
  b: float;
}
[@@deriving twine, show]

let foo1 = { a = 7; b = 2.0 }
let s = Imandrakit_twine.Encode.to_string foo_to_twine foo1;;

Printf.printf "serialized %s to %S\nhex:\n%s\n" (show_foo foo1) s
  (Hex.hexdump_s (Hex.of_string s))
;;

Printf.printf "len: %d\n" (String.length s)

let foo2 = Imandrakit_twine.Decode.decode_string foo_of_twine s;;

Format.printf "foo after roundtrip: %a@." pp_foo foo2
