type t1 =
  | F of (string[@as_bytes])
  | G of string
[@@deriving show, twine]

let x = F "hello\x000world"
let enc_x = Imandrakit_twine.Encode.to_string t1_to_twine x;;

Format.printf "x: %a@." pp_t1 x;;
Format.printf "enc(x): %S@." enc_x;;
Format.printf "len: %d@." (String.length enc_x);;
Format.printf "hex:@.%s@." (Hex.hexdump_s @@ Hex.of_string enc_x)

let x' = Imandrakit_twine.Decode.decode_string t1_of_twine enc_x;;

Format.printf "deser: %a@." pp_t1 x'

type t2 = { t1s: t1 option list } [@@deriving show, twine]

let t2 = { t1s = [ Some x; None; Some x ] }
let enc_t2 = Imandrakit_twine.Encode.to_string t2_to_twine t2;;

Format.printf "t2: %a@." pp_t2 t2;;
Format.printf "enc(t2): %S@." enc_t2;;
Format.printf "len: %d@." (String.length enc_t2);;
Format.printf "hex:@.%s@." (Hex.hexdump_s @@ Hex.of_string enc_t2)

let t2' = Imandrakit_twine.Decode.decode_string t2_of_twine enc_t2;;

Format.printf "deser: %a@." pp_t2 t2'
