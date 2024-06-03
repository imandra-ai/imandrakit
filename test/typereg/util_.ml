let dump () =
  Imandrakit_typereg.(
    to_iter top |> Iter.to_rev_list
    |> List.sort Ty_def.compare_by_name
    |> List.iter (fun d -> Format.printf "%a@." Ty_def.pp d))
