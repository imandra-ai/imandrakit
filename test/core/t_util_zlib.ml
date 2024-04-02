include (val Imandrakit_testlib.make ~__FILE__ ())

let () =
  t ~name:"zipbig" @@ fun () ->
  let s = String.make 50_000_000 'c' in
  let s1 = Util_zlib.compress_str s in
  let s2 = Util_zlib.decompress_str s1 in
  assert_equal ~printer:(spf "%S") s s2;
  true

let () =
  t ~name:"zipbig_slice" @@ fun () ->
  let s = String.make 50_000_000 'c' in
  let s1 = Util_zlib.compress_str s in
  let s2 =
    Util_zlib.decompress_slice (Bytes.unsafe_of_string s1) 0 (String.length s1)
    |> Bytes.unsafe_to_string
  in
  assert_equal ~printer:(spf "%S") s s2;
  true
