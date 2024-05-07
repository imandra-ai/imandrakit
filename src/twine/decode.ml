open Types
module Slice = Byte_slice
module LEB128 = Imandrakit_leb128.Decode

type immediate = Immediate.t
type t = { sl: slice } [@@unboxed]

type array_cursor = {
  c_dec: t;
  mutable c_num_items: int;  (** how many values *)
  mutable c_offset: offset;  (** Offset to the next value *)
}

let[@inline] create sl : t = { sl }

type 'a decoder = t -> offset -> 'a
type num_bytes_consumed = int

module Value = struct
  type cstor_index = int
  type nonrec array_cursor = array_cursor

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
- 12: cstorN, constructor index is [n], length as LEB128 follows (probably just one byte)
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

  let[@inline] length (self : t) = self.c_num_items

  let[@inline] next (self : t) : Value.t =
    if self.c_num_items <= 0 then fail "Twine: cursor: no more elements";
    let v, n_bytes = read_ self.c_dec self.c_offset in
    self.c_num_items <- self.c_num_items - 1;
    self.c_offset <- self.c_offset + n_bytes;
    v
end

(*
let tag (self : decoder) (off : offset) : tag =
  let c = Slice.get self.sl off in
  match c with
  | 'T' -> True
  | 'F' -> False
  | 'N' -> Null
  | 'i' | 'I' | 'l' | 'L' -> Int
  | 'd' | 'D' -> Float
  | 's' | 'S' -> String
  | 'b' | 'B' -> Blob
  | 'K' -> Key
  | 'P' | 'p' -> Pointer
  | '{' -> Dict
  | '[' -> Array
  | 'R' -> Record
  | 'C' -> Constructor
  | _ -> failf "Twine: unknown tag %C at offset %d" c off

(** Set cursor to [len] bytes after [off] *)
let[@inline] set_cursor_ (self : decoder) off len =
  if off + len > self.sl.len then
    failf "Twine: bound check error (offset=%d, len=%d)" off len;
  self.cur_offset <- off + len

let u8_to_int = Char.code

let[@inline never] bad_tag ~expected off c =
  failf "Twine: expected %s, got tag %C at offset %d" expected c off

let[@inline] small_int_ (self : decoder) off c : int =
  match c with
  | 'i' ->
    set_cursor_ self off 2;
    u8_to_int (Slice.get self.sl (off + 1))
  | 'I' ->
    set_cursor_ self off 3;
    Bytes.get_int16_le self.sl.bs (off + 1)
  | 'l' ->
    set_cursor_ self off 5;
    Bytes.get_int32_le self.sl.bs (off + 1) |> Int32.to_int
  | _ -> assert false

let int_truncate (self : decoder) off =
  match Slice.get self.sl off with
  | ('i' | 'I' | 'l') as c -> small_int_ self off c
  | 'L' ->
    set_cursor_ self off 9;
    Bytes.get_int64_le self.sl.bs (off + 1) |> Int64.to_int
  | c -> bad_tag ~expected:"int" off c

let int64 (self : decoder) off : int64 =
  match Slice.get self.sl off with
  | ('i' | 'I' | 'l') as c -> small_int_ self off c |> Int64.of_int
  | 'L' ->
    set_cursor_ self off 9;
    Bytes.get_int64_le self.sl.bs (off + 1)
  | c -> bad_tag ~expected:"int" off c

let pointer_ (self : decoder) off c =
  match c with
  | 'p' ->
    set_cursor_ self off 3;
    let delta = Bytes.get_uint16_le self.sl.bs (off + 1) in
    if delta > off then
      failf "Twince: relative pointer (delta=%d) is out of bound at %d" delta
        off;
    off - delta
  | 'P' -> int_truncate self (off + 1)
  | _ -> bad_tag ~expected:"pointer" off c

let[@inline] pointer (self : decoder) off : offset =
  pointer_ self off (Slice.get self.sl off)

let deref_rec self off =
  let off = ref off in
  while
    match Slice.get self.sl !off with
    | ('P' | 'p') as c ->
      off := pointer_ self !off c;
      true
    | _ -> false
  do
    ()
  done;
  !off

let float (self : decoder) off : float =
  match Slice.get self.sl off with
  | 'f' ->
    set_cursor_ self off 5;
    let i32 = Bytes.get_int32_le self.sl.bs (off + 1) in
    Int32.float_of_bits i32
  | 'F' ->
    set_cursor_ self off 9;
    let i64 = Bytes.get_int64_le self.sl.bs (off + 1) in
    Int64.float_of_bits i64
  | c -> bad_tag ~expected:"float" off c

let string_or_blob_ref_ (self : decoder) off c : string * int * int =
  match c with
  | 's' | 'b' ->
    set_cursor_ self off 2;
    let len = u8_to_int (Slice.get self.sl (off + 1)) in
    set_cursor_ self off (2 + len);
    Bytes.unsafe_to_string self.sl.bs, off + 2, len
  | 'S' | 'B' ->
    let len = int_truncate self (off + 1) in
    (* offset for the beginning of the string *)
    let offset_str = self.cur_offset in
    set_cursor_ self offset_str len;
    Bytes.unsafe_to_string self.sl.bs, offset_str, len
  | _ -> assert false

let string_ref self off =
  match Slice.get self.sl off with
  | ('s' | 'S') as c -> string_or_blob_ref_ self off c
  | c -> bad_tag ~expected:"string" off c

let string self off =
  let s, i, len = string_ref self off in
  String.sub s i len

let blob_ref self off =
  match Slice.get self.sl off with
  | ('b' | 'B') as c -> string_or_blob_ref_ self off c
  | c -> bad_tag ~expected:"blob" off c

let blob self off =
  let s, i, len = blob_ref self off in
  String.sub s i len

let key_content (self : decoder) off =
  match Slice.get self.sl off with
  | 'K' ->
    set_cursor_ self off 1;
    off + 1
  | c -> bad_tag ~expected:"key" off c

let dict (self : decoder) off st1 st2 f : unit =
  match Slice.get self.sl off with
  | '{' ->
    let off = ref (off + 1) in
    while Slice.get self.sl !off != '}' do
      let name = string self !off in
      off := self.cur_offset;
      (* call with the value *)
      f st1 st2 name !off;
      off := self.cur_offset
    done
  | c -> bad_tag ~expected:"array" off c

let array (self : decoder) off st1 st2 f : unit =
  match Slice.get self.sl off with
  | '[' ->
    let off = ref (off + 1) in
    let index = ref 0 in
    while Slice.get self.sl !off != ']' do
      f st1 st2 !index !off;
      incr index;
      off := self.cur_offset
    done
  | c -> bad_tag ~expected:"array" off c

(*
let rec skip_rec (self : decoder) (off : offset) : offset =
  match Slice.get self.sl self.cur_offset with
  | 'T' | 'F' | 'N' -> off + 1
  | 'i' -> off + 2
  | 'I' -> off + 3
  | 'l' -> off + 5
  | 'L' -> off + 9
  | 'd' -> off + 5
  | 'D' -> off + 9
  | 's' | 'b' ->
    let len = u8_to_int (Slice.get self.sl (off + 1)) in
    off + 2 + len
  | 'S' | 'B' ->
    let len = int_truncate self (off + 1) in
    self.cur_offset + len
  | 'K' -> skip_rec self (off + 1)
  | 'p' -> off + 3
  | 'P' ->
    (* skip the integer *)
    skip_rec self (off + 1)
  | '{' ->
    let off = ref (off + 1) in
    while Slice.get self.sl !off != '}' do
      off := skip_rec self !off;
      off := skip_rec self !off
    done;
    !off + 1
  | '[' ->
    let off = ref (off + 1) in
    while Slice.get self.sl !off != '}' do
      off := skip_rec self !off
    done;
    !off + 1
  | 'R' ->
    let r_descr_ptr = int_truncate self (off + 1) in
    (* offset after the descriptor *)
    let off = ref self.cur_offset in
    let r = record_descr self r_descr_ptr in
    for _i = 1 to Array.length r.fs do
      off := skip_rec self !off
    done;
    !off
  | 'C' -> assert false
  | c -> bad_tag ~expected:"any value" off c
  *)

let get_or_parse_record_descr_ (self : decoder) off : record_descriptor =
  match Int_tbl.find_opt self.records off with
  | Some r -> r
  | None ->
    let res_ref = ref None in
    dict self off self res_ref (fun self res_ref name off ->
        if name = "fs" then (
          let fields = ref [] in
          array self off self fields (fun self fields _idx off ->
              let name = string self off in
              fields := name :: !fields);
          res_ref :=
            Some
              ({ fs = Array.of_list @@ List.rev !fields } : record_descriptor)
        ));
    let r =
      match !res_ref with
      | None -> failf "Failed to read a record descriptor at %d" off
      | Some r -> r
    in
    Int_tbl.add self.records off r;
    r

let record_descr (self : decoder) off : record_descriptor =
  match Slice.get self.sl off with
  | 'R' ->
    (* resolve descriptor and position ourselves after *)
    let d_ptr = int_truncate self (off + 1) in
    get_or_parse_record_descr_ self d_ptr
  | c -> bad_tag ~expected:"record" off c

let record_fields (self : decoder) off st1 st2 f : unit =
  match Slice.get self.sl off with
  | 'R' ->
    (* resolve descriptor and position ourselves after *)
    let d_ptr = int_truncate self (off + 1) in
    let off = ref self.cur_offset in
    (* get descriptor *)
    let d = get_or_parse_record_descr_ self d_ptr in

    for i = 0 to Array.length d.fs - 1 do
      let name = Array.get d.fs i in
      (* FIXME: the user has to read it!! :s *)
      f st1 st2 name !off
    done
  | c -> bad_tag ~expected:"record" off c

open struct
  type cstor_temp = {
    mutable tmp_fs: string array option;
    mutable tmp_ar: int;
  }
end

let parse_cstor_descr_ (self : decoder) off : cstor_descriptor =
  let res = { tmp_fs = None; tmp_ar = 0 } in
  dict self off self res (fun self res name off ->
      if name = "ar" then (
        let i = int_truncate self off in
        res.tmp_ar <- i
      ) else if name = "fs" then (
        let fields = ref [] in
        array self off self fields (fun self fields _idx off ->
            let name = string self off in
            fields := name :: !fields);
        res.tmp_fs <- Some (Array.of_list @@ List.rev !fields)
      ));
  ({ fs = res.tmp_fs; ar = res.tmp_ar } : cstor_descriptor)

let get_or_parse_sum_type_descriptor_ (self : decoder) off : sum_type_descriptor
    =
  match Int_tbl.find_opt self.sum_types off with
  | Some r -> r
  | None ->
    let cs = ref [||] in
    dict self off self cs (fun self cs name off ->
        if name = "cs" then (
          let cs_l = ref [] in
          array self off self cs_l (fun self cs_l _ off ->
              let c = parse_cstor_descr_ self off in
              cs_l := c :: !cs_l);
          cs := Array.of_list @@ List.rev !cs_l
        ));
    let r = { cs = !cs } in
    Int_tbl.add self.sum_types off r;
    r

let cstor_num (self : decoder) off : int =
  match Slice.get self.sl off with
  | 'C' -> u8_to_int @@ Slice.get self.sl (off + 1)
  | c -> bad_tag ~expected:"cstor" off c

let cstor_descr (self : decoder) off : cstor_descriptor =
  match Slice.get self.sl off with
  | 'C' ->
    let index = u8_to_int @@ Slice.get self.sl (off + 1) in
    let descr_ptr = int_truncate self (off + 2) in
    let off_after_descr_ptr = self.cur_offset in
    let descr = get_or_parse_sum_type_descriptor_ self descr_ptr in
    self.cur_offset <- off_after_descr_ptr;
    Array.get descr.cs index
  | c -> bad_tag ~expected:"cstor" off c

let cstor_args (self : decoder) off st1 st2 f : unit =
  let c = cstor_descr self off in
  for i = 0 to c.ar - 1 do
    (* FIXME: the user must read the value to skip it! *)
    f st1 st2 off
  done
  *)
