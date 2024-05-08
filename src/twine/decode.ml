open Types
open Common_
module Slice = Byte_slice
module LEB128 = Imandrakit_leb128.Decode

type t = { sl: slice } [@@unboxed]

type array_cursor = {
  c_dec: t;
  mutable c_num_items: int;  (** how many values *)
  mutable c_offset: offset;  (** Offset to the next value *)
}

let show_array_cursor (self : array_cursor) =
  spf "<twine.array-cursor :off=%d :num-items=%d>" self.c_offset
    self.c_num_items

let pp_array_cursor = Fmt.of_to_string show_array_cursor
let[@inline] create sl : t = { sl }

type 'a decoder = t -> offset -> 'a
type num_bytes_consumed = int

module Value = struct
  type cstor_index = int [@@deriving show]
  type nonrec array_cursor = array_cursor

  let pp_array_cursor = pp_array_cursor

  (** A value *)
  type t =
    | Null
    | True
    | False
    | Int of int64
    | Float of float
    | String of slice
    | Blob of slice
    | Pointer of offset
    | Array of array_cursor
    | Dict of array_cursor
    | Tag of int * offset
    | Cstor0 of cstor_index
    | Cstor1 of cstor_index * offset
    | CstorN of cstor_index * array_cursor
  [@@deriving show { with_path = false }]
end

(*
# Overview

The encoding relies on a first byte to disambiguate between values.
This first byte we name "initial byte".
The first byte is a bitfield [kind:4 low:4] where:
  - [kind] is the kind of value (int, float, etc.)
  - [low] is a small integer value whose meaning depends on the kind.
    It can be a small integer, or a length, or a special value,
    or a constructor index. In case the value needs an integer argument [n]
    (length, actual integer, element count, etc.),  this
    integer argument is encoded in [low], but if [n>=15] then
      [low = 15] and [n-15] is encoded as LEB128 immediately
      after the first byte.

kinds:
- 0: special.
  n=0: false
  n=1: true
  n=2: nil
  Other values of [n] are reserved.
- 1: positive int, value is [n].
- 2: negative int, value is [-n-1]
- 3: float. if [n=0] then it's a float32, 4 bytes follow (little-endian);
  if [n=1] then it's a float64, 8 bytes follow (little-endian).
  Other values of [n] are reserved.
- 4: string, length in bytes is [n]. [n] bytes follow.
- 5: binary blob, length in bytes is [n]. [n] bytes follow.
- 6: array, number of elements is [n]. Elements follow.
- 7: dict, number of key/value pairs is [n]. [2*n] elements follow.
- 8: tag, the tag number is [n]. A single value follows.
- 9: reserved
- 10: cstor0, constructor index is [n]
- 11: cstor1, constructor index is [n], a single argument follows
- 12: cstorN, constructor index is [n], length as LEB128 follows
      (probably just one byte), then arguments follow
- 13, 14: reserved
  (possible extension: 14: pointer with metadata? [n] is relative offset,
    [n2:LEB128] is pointer to metadata)
- 15: pointer, relative offset to the pointed value is [n].
  If we're at offset [i], then the pointer points to [i-n-1].

The toplevel value is written last, and to find it, a valid Twine blob
must end with a byte [n:u8] that indicates where this last value starts.
If the last byte [n] is at address [off], then the actual last value
starts at [off-n-1]. If the last value is too big, it's turned
into a pointer and that's what that last byte targets.

*)

let invalid_first_byte_ msg ~offset ~high ~low =
  failf "Decode: invalid first byte %d/%d at %d: %s" high low offset msg

let[@inline] get_char_ (self : t) (offset : offset) : int =
  Char.code (Slice.get self.sl offset)

let[@inline] get_high (c : int) : int = (c land 0b1111_0000) lsr 4
let[@inline] get_low (c : int) : int = c land 0b0000_1111

(** Read a int64 value starting at [offset] *)
let[@inline] get_int64_ self offset ~low : int64 * num_bytes_consumed =
  if low < 15 then
    Int64.of_int low, 0
  else (
    let i, n = LEB128.u64 self.sl (offset + 1) in
    Int64.add i 15L, n
  )

let[@inline] get_int_truncate_ self offset ~low : int * num_bytes_consumed =
  let i, n_bytes = get_int64_ self offset ~low in
  Int64.to_int i, n_bytes

let get_special_ offset ~high ~low : Value.t =
  match low with
  | 0 -> Value.False
  | 1 -> Value.True
  | 2 -> Value.Null
  | _ -> invalid_first_byte_ ~offset ~high ~low "expected special value"

(** Read a float value starting at [offset]. *)
let[@inline] get_float_ (self : t) offset ~low : float * num_bytes_consumed =
  let isf32 = low = 0 in
  if isf32 then
    ( Bytes.get_int32_le self.sl.bs (self.sl.off + offset) |> Int32.float_of_bits,
      4 )
  else
    ( Bytes.get_int64_le self.sl.bs (self.sl.off + offset) |> Int64.float_of_bits,
      8 )

(* TODO: this should also return the number of bytes used, and be what we use in the array cursor *)
let read_ (self : t) (offset : offset) : Value.t * num_bytes_consumed =
  let c = get_char_ self offset in
  let high = get_high c in
  let low = get_low c in
  match high with
  | 0 ->
    let v = get_special_ offset ~high ~low in
    v, 1
  | 1 ->
    let i, n = get_int64_ self offset ~low in
    Value.Int i, n + 1
  | 2 ->
    let i, n = get_int64_ self offset ~low in
    (* [-i-1] *)
    Value.Int Int64.(sub (neg i) 1L), n + 1
  | 3 ->
    let f, n = get_float_ self offset ~low in
    Value.Float f, n + 1
  | 4 ->
    let len, size_len = get_int_truncate_ self offset ~low in
    let s = Slice.sub self.sl (offset + 1 + size_len) len in
    Value.String s, 1 + size_len + len
  | 5 ->
    let len, size_len = get_int_truncate_ self offset ~low in
    let s = Slice.sub self.sl (offset + 1 + size_len) len in
    Value.Blob s, 1 + size_len + len
  | 6 ->
    let len, size_len = get_int_truncate_ self offset ~low in
    let c : array_cursor =
      { c_offset = offset + 1 + size_len; c_num_items = len; c_dec = self }
    in
    Value.Array c, -1
  | 7 ->
    let len, size_len = get_int_truncate_ self offset ~low in
    let c : array_cursor =
      { c_offset = offset + 1 + size_len; c_num_items = 2 * len; c_dec = self }
    in
    Value.Dict c, -1
  | 8 ->
    let n, size_n = get_int_truncate_ self offset ~low in
    Value.Tag (n, offset + 1 + size_n), -1
  | 9 -> invalid_first_byte_ ~offset ~high ~low "type is reserved"
  | 10 ->
    let idx_cstor, n = get_int_truncate_ self offset ~low in
    Value.Cstor0 idx_cstor, 1 + n
  | 11 ->
    let idx_cstor, size_idx_cstor = get_int_truncate_ self offset ~low in
    Value.Cstor1 (idx_cstor, offset + 1 + size_idx_cstor), 1 + size_idx_cstor
  | 12 ->
    let idx_cstor, size_idx_cstor = get_int_truncate_ self offset ~low in
    let offset_after_n = offset + 1 + size_idx_cstor in
    let num_items, size_num_items =
      LEB128.uint_truncate self.sl offset_after_n
    in
    let c : array_cursor =
      {
        c_dec = self;
        c_num_items = num_items;
        c_offset = offset_after_n + size_num_items;
      }
    in
    Value.CstorN (idx_cstor, c), -1
  | 13 | 14 -> invalid_first_byte_ ~offset ~high ~low "type is reserved"
  | 15 ->
    let n, size_n = get_int_truncate_ self offset ~low in
    Value.Pointer n, size_n + 1
  | _ -> assert false

let[@inline] read self offset : Value.t =
  let v, _ = read_ self offset in
  v

let deref_rec self off : offset =
  let off = ref off in
  while
    let c = get_char_ self !off in
    let high = get_high c in
    if high = 15 then (
      let low = get_low c in
      let p, _ = get_int_truncate_ self !off ~low in
      off := !off - p - 1;
      true
    ) else
      false
  do
    ()
  done;
  !off

module Array_cursor = struct
  type t = array_cursor

  let pp = pp_array_cursor
  let show = show_array_cursor
  let[@inline] length (self : t) = self.c_num_items

  let[@inline] next (self : t) : Value.t =
    if self.c_num_items <= 0 then fail "Twine: cursor: no more elements";
    let v, n_bytes = read_ self.c_dec self.c_offset in
    self.c_num_items <- self.c_num_items - 1;
    self.c_offset <- self.c_offset + n_bytes;
    v
end
