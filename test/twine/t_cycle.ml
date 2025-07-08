let str = "61f001" |> CCString.of_hex_exn

let () =
  try
    ignore (Imandrakit_twine.Dump.dump_string str : string);
    assert false
  with exn -> Printf.printf "failed: %s\n%!" (Printexc.to_string exn)
