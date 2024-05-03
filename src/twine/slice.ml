type t = {
  bs: bytes;
  len: int;
}

let create ?len bs =
  let len =
    match len with
    | None -> Bytes.length bs
    | Some n ->
      assert (n >= 0 && n <= Bytes.length bs);
      n
  in
  { bs; len }

let[@inline] of_string s = create (Bytes.unsafe_of_string s)

let[@inline] get self off : char =
  if off >= self.len then invalid_arg "Twine: out of bound access";
  Bytes.unsafe_get self.bs off
