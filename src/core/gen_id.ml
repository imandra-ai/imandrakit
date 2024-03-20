(** Generate integer IDs *)

module type S = sig
  (** Generative identifiers for groups *)

  type t = private int [@@deriving eq, ord, show]

  val hash : t -> int

  val create : unit -> t

  module Tbl : CCHashtbl.S with type key = t

  module Map : CCMap.S with type key = t

  module Set : CCSet.S with type elt = t

  val pp_set : Set.t Fmt.printer
end

module Make () : S = struct
  type t = int [@@deriving show, eq, ord]

  let hash = CCHash.int

  let count_ = Atomic.make 0

  let create () = Atomic.fetch_and_add count_ 1

  module Tbl = Int_tbl
  module Set = Int_set
  module Map = Int_map

  let pp_set =
    Fmt.(
      within "{" "}" @@ hovbox @@ map Set.to_iter @@ Util.pp_iter ~sep:", " pp)
end
