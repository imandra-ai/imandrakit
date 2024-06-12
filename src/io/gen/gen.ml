let after_4_13 = {|
let realpath = Unix.realpath 
|}

let before_4_13 = {|
let realpath x = x
|}

let () =
  let major, min = Scanf.sscanf Sys.ocaml_version "%d.%d" (fun x y -> x, y) in
  if (major, min) >= (4, 13) then
    print_endline after_4_13
  else
    print_endline before_4_13
