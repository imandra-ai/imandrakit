open Util_

(* test that typereg.name works to disambiguate types *)

module A = struct
  type t = int [@@deriving typereg] [@@typereg.name "A.t"]
end

module B = struct
  type t = bool [@@deriving typereg] [@@typereg.name "B.t"]
end

type u = { x: float } [@@unboxed] [@@deriving typereg]

let () =
  print_endline "t3:";
  dump ()
