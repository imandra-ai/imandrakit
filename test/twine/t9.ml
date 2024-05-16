module Foo : sig
  type t [@@deriving twine]

  val some_t : t

  type t2 = { foo: int } [@@deriving twine]
end = struct
  type t = int [@@deriving twine]

  let some_t = 42

  type t2 = { foo: int } [@@deriving twine]
end

let () =
  let s = Imandrakit_twine.Encode.encode_to_string Foo.to_twine Foo.some_t in
  assert (Foo.some_t = Imandrakit_twine.Decode.decode_string Foo.of_twine s)

let () =
  let s =
    Imandrakit_twine.Encode.encode_to_string Foo.t2_to_twine { foo = 41 }
  in
  assert (
    { Foo.foo = 41 } = Imandrakit_twine.Decode.decode_string Foo.t2_of_twine s)
