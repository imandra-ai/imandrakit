open! Types

(**/**)

let[@inline] fail msg = raise (Error msg)
let[@inline] failf msg = Printf.ksprintf fail msg

(**/**)
