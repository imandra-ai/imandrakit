let ( let@ ) = ( @@ )

let compress ?(buf = Buffer.create 32) (s : bytes) : bytes =
  let@ _sp = Trace_core.with_span ~__FILE__ ~__LINE__ "zlib.compress" in
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

let decompress_slice ?(buf = Buffer.create 32) (s : bytes) offset len : bytes =
  let@ _sp = Trace_core.with_span ~__FILE__ ~__LINE__ "zlib.decompress" in
  if offset + len > Bytes.length s then invalid_arg "Util_zlib.decompress";
  Buffer.clear buf;
  let i = ref offset in
  let last = offset + len in
  Zlib.uncompress ~header:false
    (fun bytes ->
      let len = min (last - !i) (Bytes.length bytes) in
      if len > 0 then (
        Bytes.blit s !i bytes 0 len;
        i := !i + len
      );
      len)
    (fun bytes len -> Buffer.add_subbytes buf bytes 0 len);
  Buffer.contents buf |> Bytes.unsafe_of_string

let decompress ?buf s : bytes = decompress_slice ?buf s 0 (Bytes.length s)

let decompress_str ?buf s : string =
  decompress ?buf (Bytes.unsafe_of_string s) |> Bytes.unsafe_to_string
