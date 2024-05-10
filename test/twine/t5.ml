type t = unit [@@deriving twine]

let m : string = Imandrakit_twine.Encode.to_string to_twine ()
let () = Format.printf "m:@.%s@." (Hex.hexdump_s @@ Hex.of_string m)
let () = Format.printf "size of empty msg is %d@." (String.length m)
