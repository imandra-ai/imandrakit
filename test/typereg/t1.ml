open Util_

type foo = int [@@deriving typereg]
type bar = foo * int list option [@@deriving typereg]

module Yolo = struct
  type t = float option list array
  [@@deriving typereg] [@@typereg.name "Yolo.t"]
end

let () =
  print_endline "t1:";
  dump ()
