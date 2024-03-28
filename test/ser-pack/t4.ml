type t1 =
  | F of (string[@as_bytes])
  | G of string
[@@deriving show, serpack]

let x = Imandrakit_ser_pack.to_cbor_string t1_to_serpack (F "hello\x000world");;

Format.printf "x: %S@." x;;
Format.printf "len: %d@." (String.length x)

let t1' = Imandrakit_ser_pack.of_cbor_string_exn t1_of_serpack x;;

Format.printf "deser: %a@." pp_t1 t1'
