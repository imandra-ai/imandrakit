[@@@warning "-60"]

module M1 = struct
  Format.printf "without hashcons@."

  type t0 = {
    x: int; [@key "X"]
    y: bool; [@key "Y"]
  }
  [@@deriving serpack, show]

  type t1 = { foos: t0 list } [@@deriving serpack, show]

  let myt1 =
    let x1 = { x = 0; y = false } in
    let x2 = { x = 2; y = true } in
    { foos = [ x1; x1; x2; x1; x2 ] }
  ;;

  Format.printf "myt1: %a@." pp_t1 myt1

  let pack1 = Imandrakit_ser_pack.to_value t1_to_serpack myt1;;

  Format.printf "pack1: %a@." Imandrakit_ser_pack.pp_diagnostic pack1

  let len1 =
    Imandrakit_ser_pack.to_cbor_string t1_to_serpack myt1 |> String.length
  ;;

  Format.printf "len1: %d@." len1
end

module M2 = struct
  Format.printf "with hashcons@."

  type t0 = {
    x: int; [@key "x0"]
    y: bool; [@key "y"]
  }
  [@@deriving serpack, show] [@@serpack.hashcons]

  type t1 = { foos: t0 list } [@@deriving serpack, show]

  let myt1 =
    let x1 = { x = 0; y = false } in
    let x2 = { x = 2; y = true } in
    { foos = [ x1; x1; x2; x1; x2 ] }
  ;;

  Format.printf "myt1': %a@." pp_t1 myt1

  let pack1 = Imandrakit_ser_pack.to_value t1_to_serpack myt1;;

  Format.printf "pack1: %a@." Imandrakit_ser_pack.pp_diagnostic pack1

  let len1 =
    Imandrakit_ser_pack.to_cbor_string t1_to_serpack myt1 |> String.length
  ;;

  Format.printf "len1: %d@." len1
end
