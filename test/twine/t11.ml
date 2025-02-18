type bar =
  | Bar1
  | Bar2
[@@deriving eq, show, twine]

type event =
  | E_bar of bar
  | E_prev of event Imandrakit_twine.offset_for
[@@deriving show { with_path = false }, twine]

type foo = { evs: event list } [@@deriving show { with_path = false }, twine]

let encoded : string =
  let enc = Imandrakit_twine.Encode.create () in
  let ev0 =
    Imandrakit_twine.Encode.write_offset_for enc event_to_twine (E_bar Bar1)
  in
  let ev1 =
    Imandrakit_twine.Encode.write_offset_for enc event_to_twine (E_bar Bar2)
  in
  let top = foo_to_twine enc { evs = [ E_prev ev0; E_prev ev1 ] } in
  Imandrakit_twine.Encode.finalize_copy enc ~entrypoint:top

let () =
  Format.printf "serialized:@.%s@." @@ Imandrakit_twine.Dump.dump_string encoded
