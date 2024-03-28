type foo = {
  a: int;
  b: float;
}
[@@deriving serpack, show]

let s = Imandrakit_ser_pack.to_cbor_string foo_to_serpack { a = 1; b = 2.0 };;

Printf.printf "serialized to %S\n" s;;
Printf.printf "len: %d\n" (String.length s)

let foo2 = Imandrakit_ser_pack.of_cbor_string_exn foo_of_serpack s;;

Format.printf "foo after roundtrip: %a@." pp_foo foo2
