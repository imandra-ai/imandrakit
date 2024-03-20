let compress ?(buf = Buffer.create 32) (s : bytes) : bytes =
  Buffer.clear buf;
  let addbuf, terminate =
    Zlib.compress_direct ~header:false (fun bytes len ->
        Buffer.add_subbytes buf bytes 0 len)
  in
  addbuf s 0 (Bytes.length s);
  terminate ();
  Buffer.contents buf |> Bytes.unsafe_of_string

let compress_str ?buf s : string =
  compress ?buf (Bytes.unsafe_of_string s) |> Bytes.unsafe_to_string

let decompress ?(buf = Buffer.create 32) (s : bytes) : bytes =
  Buffer.clear buf;
  let i = ref 0 in
  Zlib.uncompress ~header:false
    (fun bytes ->
      let len = min (Bytes.length s - !i) (Bytes.length bytes) in
      if len > 0 then (
        Bytes.blit s !i bytes 0 len;
        i := !i + len
      );
      len)
    (fun bytes len -> Buffer.add_subbytes buf bytes 0 len);
  Buffer.contents buf |> Bytes.unsafe_of_string

let decompress_str ?buf s : string =
  decompress ?buf (Bytes.unsafe_of_string s) |> Bytes.unsafe_to_string
