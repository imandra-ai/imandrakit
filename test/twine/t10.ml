type foo =
  | A
  | B of { x: int }
[@@deriving twine, eq, show]

let x = B { x = 42 }
let x_str = Imandrakit_twine.Encode.encode_to_string foo_to_twine x
let x' = Imandrakit_twine.Decode.decode_string foo_of_twine x_str

let () =
  Format.printf "serialized %a is:@.%s@." pp_foo x
  @@ Imandrakit_twine.Dump.dump_string x_str

let () = assert (equal_foo x x')
