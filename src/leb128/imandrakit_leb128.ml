(* adapted from ocaml-protoc from code by c-cube *)

module Decode = struct
  module Slice = Byte_slice

  let u64 (sl : Slice.t) : int64 =
    let shift = ref 0 in
    let res = ref 0L in
    let continue = ref true in

    while !continue do
      if sl.len <= 0 then invalid_arg "out of bound";
      let b = Char.code (Bytes.get sl.bs sl.off) in
      let cur = b land 0x7f in
      if cur <> b then (
        (* at least one byte follows this one *)
        (res := Int64.(logor !res (shift_left (of_int cur) !shift)));
        sl.off <- sl.off + 1;
        sl.len <- sl.len - 1;
        shift := !shift + 7
      ) else if !shift < 63 || b land 0x7f <= 1 then (
        (res := Int64.(logor !res (shift_left (of_int b) !shift)));
        continue := false
      ) else
        invalid_arg "leb128 varint is too long"
    done;

    !res

  let[@inline] decode_zigzag (v : int64) : int64 =
    Int64.(logxor (shift_right v 1) (neg (logand v Int64.one)))

  let[@inline] i64 sl : int64 = decode_zigzag (u64 sl)
  let[@inline] int sl : int = Int64.to_int (decode_zigzag (u64 sl))
end

module Encode = struct
  module Buf = Byte_buf

  let[@inline] encode_zigzag (i : int64) : int64 =
    Int64.(logxor (shift_left i 1) (shift_right i 63))

  external varint_size : (int64[@unboxed]) -> int
    = "caml_ix_leb128_varint_size_byte" "caml_ix_leb128_varint_size"
    [@@noalloc]
  (** Compute how many bytes this int would occupy as varint *)

  external varint_slice : bytes -> (int[@untagged]) -> (int64[@unboxed]) -> unit
    = "caml_ix_leb128_varint_byte" "caml_ix_leb128_varint"
    [@@noalloc]
  (** Write this int as varint into the given slice *)

  let[@inline] u64 (buf : Buf.t) (i : int64) =
    let n = varint_size i in
    Buf.ensure_cap buf n;
    assert (buf.len + n <= Bytes.length buf.bs);
    varint_slice buf.bs buf.len i;
    buf.len <- buf.len + n

  let[@inline] i64 buf i : unit = u64 buf (encode_zigzag i)
  let[@inline] int buf i : unit = u64 buf (encode_zigzag (Int64.of_int i))
end
