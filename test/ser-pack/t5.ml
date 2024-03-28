type t = unit [@@deriving serpack]

let m = Imandrakit_ser_pack.to_cbor_string to_serpack ();;

Format.printf "size of empty msg is %d@." (String.length m)
