type const =
  | C_nativeint of nativeint
  | C_int32 of int32
  | C_int64 of int64
  | C_float of float
  | C_char of char
  | C_string of string
  | C_bytes of (string[@as_bytes])
[@@deriving twine, show, eq]

type t = { cs: const list } [@@deriving twine, show, eq]

let t0 =
  {
    cs =
      [
        C_nativeint 42n;
        C_int32 42l;
        C_int64 42L;
        C_float 12.1;
        C_char 'c';
        C_string "hello";
        C_bytes "\000\002\xff";
      ];
  }

let () = Format.printf "start with %a@." pp t0
let s = Imandrakit_twine.Encode.to_string to_twine t0
let () = Format.printf "result is:@.%s@." (Hex.hexdump_s @@ Hex.of_string s);;

Format.printf "len=%d@." (String.length s);;
Format.printf "len if marshalled=%d@." (String.length @@ Marshal.to_string t0 [])

let t0' = Imandrakit_twine.Decode.decode_string of_twine s;;

Format.printf "deser: %a@." pp t0'

let () = assert (equal t0 t0')
