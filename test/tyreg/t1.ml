open Util_

type foo = int [@@deriving tyreg]
type bar = foo * int list option [@@deriving tyreg]

module Yolo = struct
  type t = float option list array [@@deriving tyreg]
end

let () =
  print_endline "t1:";
  dump ()
