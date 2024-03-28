type 'a foo = {
  x: 'a option;
  y: (int * 'a) list;
}
[@@deriving serpack, show]

type ('a, 'b) bar = {
  f1: 'a foo list;
  f2: 'b foo list;
}
[@@deriving serpack, show]

type bar2 = (int, bool) bar [@@deriving show, serpack]

let b : bar2 =
  {
    f1 = [ { x = Some 1; y = [ 0, 0; 1, 1; 2, 2 ] } ];
    f2 = [ { x = None; y = [] }; { x = Some true; y = [ 0, true; 1, false ] } ];
  }
;;

Format.printf "b: %a@." pp_bar2 b

let s = Imandrakit_ser_pack.to_cbor_string bar2_to_serpack b;;

Format.printf "len=%d@." (String.length s)

let b' = Imandrakit_ser_pack.of_cbor_string_exn bar2_of_serpack s;;

Format.printf "b': %a@." pp_bar2 b';;
assert (b = b')
