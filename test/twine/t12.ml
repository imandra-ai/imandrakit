(* TODO: write a long suite of "hello%d" strings with references to previous string, with and without
    a [out], check that results are the same *)

let spf = Printf.sprintf

type foo = {
  str: string;
  prev: foo Imandrakit_twine.offset_for option;
}
[@@deriving eq, show, twine]

type foos = { foos: foo Imandrakit_twine.offset_for list }
[@@unboxed] [@@deriving eq, show, twine]

let test ~n_items () =
  let open struct
    let encoded_normal : string =
      let enc = Imandrakit_twine.Encode.create () in
      let prev = ref None in
      let foos =
        List.init n_items (fun i ->
            let f = { str = spf "hello %d" i; prev = !prev } in
            let f_ptr =
              Imandrakit_twine.Encode.write_offset_for enc foo_to_twine f
            in
            prev := Some f_ptr;
            f_ptr)
      in
      let entrypoint = foos_to_twine enc { foos } in
      Imandrakit_twine.Encode.finalize_copy enc ~entrypoint

    let encoded_flush : string =
      let buf = Buffer.create 32 in
      let out : Imandrakit_twine.Encode.out =
        object
          method output bs i len = Buffer.add_subbytes buf bs i len
        end
      in

      let enc = Imandrakit_twine.Encode.create () in
      let prev = ref None in
      let foos =
        List.init n_items (fun i ->
            let f = { str = spf "hello %d" i; prev = !prev } in
            let f_ptr =
              Imandrakit_twine.Encode.write_offset_for enc foo_to_twine f
            in
            prev := Some f_ptr;

            (* flush *)
            Imandrakit_twine.Encode.write_internal_data enc out;
            assert (Imandrakit_twine.Encode.internal_size enc = 0);

            f_ptr)
      in
      let entrypoint = foos_to_twine enc { foos } in
      Buffer.add_string buf
      @@ Imandrakit_twine.Encode.finalize_copy enc ~entrypoint;
      Buffer.contents buf

    let () =
      Printf.printf "normal.size=%d, flush.size=%d\n%!"
        (String.length encoded_normal)
        (String.length encoded_flush);
      if encoded_normal <> encoded_flush then (
        Printf.printf "distinct!\n%!";
        Printf.printf "normal:\n%s\n"
        @@ Hex.hexdump_s (Hex.of_string encoded_normal);

        Printf.printf "with flush:\n%s\n"
        @@ Hex.hexdump_s (Hex.of_string encoded_flush);

        Printf.printf "%!";
        assert false
      );

      let decoded_normal =
        Imandrakit_twine.Decode.decode_string foos_of_twine encoded_normal
      in
      let decoded_flush =
        Imandrakit_twine.Decode.decode_string foos_of_twine encoded_flush
      in
      assert (decoded_normal = decoded_flush)
  end in
  ()

let () = test ~n_items:10 ()
let () = test ~n_items:1_000 ()
let () = test ~n_items:100_000 ()
