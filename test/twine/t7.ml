type 'a foo = {
  x: 'a option;
  y: (int * 'a) list;
}
[@@deriving twine, show, eq]

type ('a, 'b) bar = {
  f1: 'a foo list;
  f2: 'b foo list;
}
[@@deriving twine, show, eq]

type bar2 = (int, bool) bar [@@deriving show, twine, eq]

let b : bar2 =
  {
    f1 = [ { x = Some 1; y = [ 0, 0; 1, 1; 2, 2 ] } ];
    f2 = [ { x = None; y = [] }; { x = Some true; y = [ 0, true; 1, false ] } ];
  }
;;

Format.printf "b: %a@." pp_bar2 b

let s = Imandrakit_twine.Encode.to_string bar2_to_twine b;;

Format.printf "len=%d@." (String.length s)

let b' = Imandrakit_twine.Decode.decode_string bar2_of_twine s;;

Format.printf "b': %a@." pp_bar2 b';;
assert (equal_bar2 b b')
