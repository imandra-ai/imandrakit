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
        | CstorN (0, c) when Array_cursor.length c = 1 ->
          let x = p1 dec (Array_cursor.current c) in
          Ok x
        | CstorN (1, c) when Array_cursor.length c = 1 ->
          let e = p2 dec (Array_cursor.current c) in
          Error e
        | _ -> fail "expected result")
end
