open Util_

[@@@ocaml.warning "-37-69"]

type foo = {
  x: int;
  y: float option;
}
[@@deriving typereg]

type bar =
  | A
  | B of int
  | C of int * string
  | D of {
      x: (int[@twine_skip_field]);
      y: (string * foo) list;
    }
[@@deriving typereg]

let () =
  print_endline "t2:";
  dump ()
