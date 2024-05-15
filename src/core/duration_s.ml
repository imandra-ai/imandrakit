(** Duration, in seconds *)

type t = (float[@printer Util.pp_duration_s]) [@@deriving show, serpack, twine]
