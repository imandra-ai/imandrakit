open Util_

type foo = {
  x: int;
  y: float option;
}
[@@deriving tyreg]

type bar =
  | A
  | B of int
  | C of int * string
  | D of {
      x: int;
      y: (string * foo) list;
    }
[@@deriving tyreg]

let () =
  print_endline "t2:";
  dump ()
