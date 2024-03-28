type const =
  | C_nativeint of nativeint
  | C_int32 of int32
  | C_int64 of int64
  | C_float of float
  | C_char of char
  | C_string of string
  | C_bytes of (string[@as_bytes])
[@@deriving serpack, show]

type t = { cs: const list } [@@deriving serpack, show]

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

let s = Imandrakit_ser_pack.to_cbor_string to_serpack t0
let cbor = Imandrakit_ser_pack.to_value to_serpack t0;;

Format.printf "result is %S@." s;;
Format.printf "as cbor: %a@." Imandrakit_ser_pack.pp_diagnostic cbor;;
Format.printf "len=%d@." (String.length s);;
Format.printf "len if marshalled=%d@." (String.length @@ Marshal.to_string t0 [])

let t0' = Imandrakit_ser_pack.of_cbor_string_exn of_serpack s;;

Format.printf "deser: %a@." pp t0'
