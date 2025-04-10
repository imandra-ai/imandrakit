(** Common aliases and basic functions *)

module Fmt = CCFormat
module Atomic = Atomic
module Vec = CCVector
module Trace = Trace_core

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

(**/**)

(** Globals related to the current process *)
module Global_process_data = struct
  open struct
    let process_name_ = ref (Filename.basename Sys.argv.(0))
  end

  let set_process_name s = process_name_ := s
  let get_process_name () = !process_name_
end

(**/**)
