module Result = struct
  let iter_result fok ferr = function
    | Ok x -> fok x
    | Error e -> ferr e

  type ('a, 'err) t = ('a, 'err) result [@@deriving show, map, iter]

  let to_twine p1 p2 enc = function
    | Ok x -> Encode.(cstor enc ~index:0 [| p1 enc x |])
    | Error e -> Encode.(cstor enc ~index:1 [| p2 enc e |])

  let of_twine p1 p2 : _ Decode.decoder =
    Decode.(
      fun dec c ->
        match read dec c with
        | Cstor1 (0, off) ->
          let x = p1 dec off in
          Ok x
        | Cstor1 (1, off) ->
          let e = p2 dec off in
          Error e
        | _ -> fail "expected result")
end
