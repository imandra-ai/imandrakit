[@@@warning "-60"]

module M1 = struct
  Format.printf "without hashcons@."

  type t0 = {
    x: int; [@key "X"]
    y: bool; [@key "Y"]
  }
  [@@deriving twine, show]

  type t1 = { foos: t0 list } [@@deriving twine, show]

  let myt1 =
    let x1 = { x = 0; y = false } in
    let x2 = { x = 2; y = true } in
    { foos = [ x1; x1; x2; x1; x2 ] }
  ;;

  Format.printf "myt1: %a@." pp_t1 myt1

  let pack1 = Imandrakit_twine.Encode.to_string t1_to_twine myt1
  let () = Format.printf "pack1:@.%s@." (Hex.hexdump_s @@ Hex.of_string pack1)
  let () = Format.printf "len1: %d@." @@ String.length pack1
end

module M2 = struct
  Format.printf "with caching@."

  type t0 = {
    x: int; [@key "x0"]
    y: bool; [@key "y"]
  }
  [@@deriving twine, show, eq]

  module T0 = struct
    type t = t0 [@@deriving eq]

    let hash = Hashtbl.hash
  end

  type t1 = { foos: t0 list } [@@deriving twine, show]

  let () = Imandrakit_twine.Encode.add_cache (module T0) t0_to_twine_ref

  let myt1 =
    let x1 = { x = 0; y = false } in
    let x2 = { x = 2; y = true } in
    { foos = [ x1; x1; x2; x1; x2 ] }
  ;;

  Format.printf "myt1': %a@." pp_t1 myt1

  let pack1 = Imandrakit_twine.Encode.to_string t1_to_twine myt1
  let () = Format.printf "pack1:@.%s@." (Hex.hexdump_s @@ Hex.of_string pack1)
  let () = Format.printf "len1: %d@." (String.length pack1)
end
