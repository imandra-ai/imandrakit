(** Common aliases and  basic functions *)

module Fmt = CCFormat
module Atomic = Atomic
module Vec = CCVector

let ( let@ ) = ( @@ )
let spf = Printf.sprintf

(** list with spaces **)
let pp_list_sp pp = Fmt.list ~sep:(Fmt.return "@ ") pp

(** {2 Various instances} *)

module Str_tbl = CCHashtbl.Make (CCString)
module Str_map = CCMap.Make (CCString)
module Str_set = CCSet.Make (CCString)
module Int_tbl = CCHashtbl.Make (CCInt)
module Int_map = CCMap.Make (CCInt)
module Int_set = CCSet.Make (CCInt)
