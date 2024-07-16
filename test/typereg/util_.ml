let dump () =
  Imandrakit_typereg.(
    to_iter top |> Iter.to_rev_list
    |> List.sort (CCList.compare Ty_def.compare_by_name)
    |> List.iter (fun d ->
           Format.printf "%a@." (CCFormat.Dump.list Ty_def.pp) d))
