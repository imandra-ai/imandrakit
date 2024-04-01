module Ser_pack = Imandrakit_ser_pack

type t = { name: string } [@@unboxed] [@@deriving show, ord, eq]

let hash self = CCHash.string self.name
let make ~name () : t = { name }
let to_serpack _st self = Ser_pack.Ser.string self.name

let of_serpack st v =
  let name = Ser_pack.Deser.to_text st v in
  make ~name ()

let generic_internal_error : t = make ~name:"GenericInternalError" ()
let timeout : t = make ~name:"Timeout" ()

module As_key = struct
  type nonrec t = t

  let equal = equal
  let compare = compare
  let hash = hash
end

module Tbl = CCHashtbl.Make (As_key)
module Map = CCMap.Make (As_key)
module Set = CCSet.Make (As_key)
