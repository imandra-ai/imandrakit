type t = {
  bs: bytes;
  mutable off: int;
  mutable len: int;
}

let create ?(off = 0) ?len bs =
  let len =
    match len with
    | None -> Bytes.length bs - off
    | Some n ->
      if n < 0 || off + n > Bytes.length bs then
        invalid_arg "Bslice: invalid length";
      n
  in
  { bs; off; len }

let[@inline] of_string s = create (Bytes.unsafe_of_string s)
let[@inline] len self = self.len

let[@inline] clear self =
  self.len <- 0;
  self.off <- 0

let[@inline] get self i : char =
  if i >= self.len then invalid_arg "Bslice: out of bound access";
  Bytes.unsafe_get self.bs (self.off + i)

let[@inline] set self i c : unit =
  if i >= self.len then invalid_arg "Bslice: out of bound access";
  Bytes.unsafe_set self.bs (self.off + i) c

let sub self off len =
  if off + len > self.len then invalid_arg "Bslice: invalid length";
  { bs = self.bs; off = self.off + off; len }

let[@inline] consume self n : unit =
  if n > self.len then invalid_arg "Bslice: consuming too many bytes";
  self.off <- self.off + n;
  self.len <- self.len - n
