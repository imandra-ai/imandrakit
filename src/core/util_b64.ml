(** Base64 *)

let encode_str (str : string) : string =
  Base64.encode_string ~alphabet:Base64.uri_safe_alphabet str

let decode_str_exn str : string =
  Base64.decode_exn ~alphabet:Base64.uri_safe_alphabet str
