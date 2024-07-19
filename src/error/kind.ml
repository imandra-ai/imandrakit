module Twine = Imandrakit_twine

type t = { name: string } [@@unboxed] [@@deriving show, ord, eq, typereg]

let[@inline] hash self = CCHash.string self.name
let make ~name () : t = { name }
let[@inline] name self = self.name
let to_twine _st self = Twine.Immediate.string self.name

let of_twine st v =
  let name = Twine.Decode.string st v in
  make ~name ()

let generic_internal_error : t = make ~name:"GenericInternalError" ()
let timeout : t = make ~name:"Timeout" ()
let todo : t = make ~name:"Todo" ()

module As_key = struct
  type nonrec t = t

  let equal = equal
  let compare = compare
  let hash = hash
end

module Tbl = CCHashtbl.Make (As_key)
module Map = CCMap.Make (As_key)
module Set = CCSet.Make (As_key)
