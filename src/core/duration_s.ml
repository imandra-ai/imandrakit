(** Duration, in seconds *)

type t = (float[@printer Util.pp_duration_s]) [@@deriving show, serpack, twine]

let ns : t = 1e-9
let us : t = 1e-6
let ms : t = 1e-3
let seconds : t = 1.
let minutes : t = 60.
let hours : t = 60. *. minutes
let days : t = 24. *. hours
