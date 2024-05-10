open Types
open Common_
module Slice = Byte_slice
module LEB128 = Imandrakit_leb128.Decode

type cached = ..

module Offset_tbl = Int_tbl

module type CACHE_KEY = sig
  type elt
  type cached += C of elt
end

type t = {
  sl: slice;
  cache: cached Offset_tbl.t;
}

type cstor_index = int [@@deriving show]

type cursor = {
  c_dec: t;
  mutable c_num_items: int;  (** how many values or pairs *)
  mutable c_offset: offset;  (** Offset to the next value *)
}

let show_cursor (self : cursor) =
  spf "<twine.cursor :off=%d :num-items=%d>" self.c_offset self.c_num_items

let pp_cursor = Fmt.of_to_string show_cursor
let[@inline] create sl : t = { sl; cache = Offset_tbl.create 8 }
let[@inline] of_string s = create @@ Slice.of_string s

type 'a decoder = t -> offset -> 'a
type num_bytes_consumed = int

module Value = struct
  type array_cursor = cursor
  type dict_cursor = cursor

  let pp_array_cursor = pp_cursor
  let pp_dict_cursor = pp_cursor

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
    | Dict of dict_cursor
    | Tag of int * offset
    | Cstor0 of cstor_index
    | Cstor1 of cstor_index * offset
    | CstorN of cstor_index * array_cursor
  [@@deriving show { with_path = false }]
end

let fail = fail
let failf = failf

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
    ( Bytes.get_int32_le self.sl.bs (self.sl.off + offset + 1)
      |> Int32.float_of_bits,
      4 )
  else
    ( Bytes.get_int64_le self.sl.bs (self.sl.off + offset + 1)
      |> Int64.float_of_bits,
      8 )

(** Number of bytes to skip *)
let skip_float_ ~low : int =
  let isf32 = low = 0 in
  if isf32 then
    5
  else
    9

let[@inline never] deref_rec_ self off : offset =
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

(* inlinable fast path for deref_rec *)
let[@inline] deref_rec self off : offset =
  let c = get_char_ self off in
  let high = get_high c in
  if high = 15 then
    (* do the actual work *)
    deref_rec_ self off
  else
    off

let read (self : t) (offset : offset) : Value.t =
  let offset = deref_rec self offset in
  let c = get_char_ self offset in
  let high = get_high c in
  let low = get_low c in
  match high with
  | 0 -> get_special_ offset ~high ~low
  | 1 ->
    let i, _ = get_int64_ self offset ~low in
    Value.Int i
  | 2 ->
    let i, _ = get_int64_ self offset ~low in
    (* [-i-1] *)
    Value.Int Int64.(sub (neg i) 1L)
  | 3 ->
    let f, _ = get_float_ self offset ~low in
    Value.Float f
  | 4 ->
    let len, size_len = get_int_truncate_ self offset ~low in
    let s = Slice.sub self.sl (offset + 1 + size_len) len in
    Value.String s
  | 5 ->
    let len, size_len = get_int_truncate_ self offset ~low in
    let s = Slice.sub self.sl (offset + 1 + size_len) len in
    Value.Blob s
  | 6 ->
    let len, size_len = get_int_truncate_ self offset ~low in
    let c : cursor =
      { c_offset = offset + 1 + size_len; c_num_items = len; c_dec = self }
    in
    Value.Array c
  | 7 ->
    let len, size_len = get_int_truncate_ self offset ~low in
    let c : cursor =
      { c_offset = offset + 1 + size_len; c_num_items = len; c_dec = self }
    in
    Value.Dict c
  | 8 ->
    let n, size_n = get_int_truncate_ self offset ~low in
    Value.Tag (n, offset + 1 + size_n)
  | 9 -> invalid_first_byte_ ~offset ~high ~low "type is reserved"
  | 10 ->
    let idx_cstor, _ = get_int_truncate_ self offset ~low in
    Value.Cstor0 idx_cstor
  | 11 ->
    let idx_cstor, size_idx_cstor = get_int_truncate_ self offset ~low in
    Value.Cstor1 (idx_cstor, offset + 1 + size_idx_cstor)
  | 12 ->
    let idx_cstor, size_idx_cstor = get_int_truncate_ self offset ~low in
    let offset_after_n = offset + 1 + size_idx_cstor in
    let num_items, size_num_items =
      LEB128.uint_truncate self.sl offset_after_n
    in
    let c : cursor =
      {
        c_dec = self;
        c_num_items = num_items;
        c_offset = offset_after_n + size_num_items;
      }
    in
    Value.CstorN (idx_cstor, c)
  | 13 | 14 -> invalid_first_byte_ ~offset ~high ~low "type is reserved"
  | 15 ->
    let n, _ = get_int_truncate_ self offset ~low in
    Value.Pointer n
  | _ -> assert false

(** Skip the current (immediate) value *)
let skip_ (self : t) (offset : offset) : num_bytes_consumed =
  let c = get_char_ self offset in
  let high = get_high c in
  let low = get_low c in
  match high with
  | 0 -> 1
  | 1 | 2 ->
    let _, n = get_int64_ self offset ~low in
    n + 1
  | 3 -> skip_float_ ~low
  | 4 | 5 ->
    let len, size_len = get_int_truncate_ self offset ~low in
    1 + size_len + len
  | 6 -> fail "Twine: decode.skip: cannot skip over array"
  | 7 -> fail "Twine: decode.skip: cannot skip over dict"
  | 8 -> fail "Twine: decode.skip: cannot skip over tag"
  | 9 -> invalid_first_byte_ ~offset ~high ~low "type is reserved"
  | 10 ->
    let _, n = get_int_truncate_ self offset ~low in
    1 + n
  | 11 -> fail "Twine: decode.skip: cannot skip over cstor1"
  | 12 -> fail "Twine: decode.skip: cannot skip over cstorN"
  | 13 | 14 -> invalid_first_byte_ ~offset ~high ~low "type is reserved"
  | 15 ->
    let _, size_n = get_int_truncate_ self offset ~low in
    size_n + 1
  | _ -> assert false

let fail_decode_type_ ~what offset =
  failf "Twine: decode: expected %s at offset=0x%x" what offset

let null self offset =
  let offset = deref_rec self offset in
  let c = get_char_ self offset in
  let high = get_high c in
  let low = get_low c in
  if high <> 0 || low <> 2 then fail_decode_type_ ~what:"null" offset;
  ()

let bool self offset =
  let offset = deref_rec self offset in
  let c = get_char_ self offset in
  let high = get_high c in
  let low = get_low c in
  match high with
  | 0 ->
    (match low with
    | 0 -> false
    | 1 -> true
    | _ -> fail_decode_type_ ~what:"bool" offset)
  | _ -> fail_decode_type_ ~what:"bool" offset

let int64 self offset =
  let offset = deref_rec self offset in
  let c = get_char_ self offset in
  let high = get_high c in
  let low = get_low c in
  match high with
  | 1 ->
    let i, _ = get_int64_ self offset ~low in
    i
  | 2 ->
    let i, _ = get_int64_ self offset ~low in
    (* [-i-1] *)
    Int64.(sub (neg i) 1L)
  | _ ->
    Printf.eprintf "high=%d, low=%d\n%!" high low;
    fail_decode_type_ ~what:"integer" offset

let[@inline] int_truncate self offset = Int64.to_int @@ int64 self offset

let float self offset =
  let offset = deref_rec self offset in
  let c = get_char_ self offset in
  let high = get_high c in
  let low = get_low c in
  if high <> 3 then fail_decode_type_ ~what:"float" offset;
  fst @@ get_float_ self offset ~low

let string_slice self offset =
  let offset = deref_rec self offset in
  let c = get_char_ self offset in
  let high = get_high c in
  let low = get_low c in
  if high <> 4 then fail_decode_type_ ~what:"string" offset;
  let len, size_len = get_int_truncate_ self offset ~low in
  Slice.sub self.sl (offset + 1 + size_len) len

let string self offset = Slice.contents @@ string_slice self offset

let blob_slice self offset =
  let offset = deref_rec self offset in
  let c = get_char_ self offset in
  let high = get_high c in
  let low = get_low c in
  if high <> 5 then fail_decode_type_ ~what:"blob" offset;
  let len, size_len = get_int_truncate_ self offset ~low in
  Slice.sub self.sl (offset + 1 + size_len) len

let blob self offset = Slice.contents @@ blob_slice self offset

let array self offset =
  let offset = deref_rec self offset in
  let c = get_char_ self offset in
  let high = get_high c in
  let low = get_low c in
  if high <> 6 then fail_decode_type_ ~what:"array" offset;
  let len, size_len = get_int_truncate_ self offset ~low in
  let c : cursor =
    { c_offset = offset + 1 + size_len; c_num_items = len; c_dec = self }
  in
  c

let dict self offset =
  let offset = deref_rec self offset in
  let c = get_char_ self offset in
  let high = get_high c in
  let low = get_low c in
  if high <> 7 then fail_decode_type_ ~what:"dict" offset;
  let len, size_len = get_int_truncate_ self offset ~low in
  let c : cursor =
    { c_offset = offset + 1 + size_len; c_num_items = len; c_dec = self }
  in
  c

let tag self offset =
  let offset = deref_rec self offset in
  let c = get_char_ self offset in
  let high = get_high c in
  let low = get_low c in
  if high <> 8 then fail_decode_type_ ~what:"tag" offset;
  let n, size_n = get_int_truncate_ self offset ~low in
  n, offset + 1 + size_n

let cstor self offset =
  let offset = deref_rec self offset in
  let c = get_char_ self offset in
  let high = get_high c in
  let low = get_low c in
  match high with
  | 10 ->
    let idx_cstor, _size_idx_cstor = get_int_truncate_ self offset ~low in
    idx_cstor, { c_offset = offset; c_num_items = 0; c_dec = self }
  | 11 ->
    let idx_cstor, size_idx_cstor = get_int_truncate_ self offset ~low in
    ( idx_cstor,
      { c_offset = offset + 1 + size_idx_cstor; c_num_items = 1; c_dec = self }
    )
  | 12 ->
    let idx_cstor, size_idx_cstor = get_int_truncate_ self offset ~low in
    let offset_after_n = offset + 1 + size_idx_cstor in
    let num_items, size_num_items =
      LEB128.uint_truncate self.sl offset_after_n
    in
    let c : cursor =
      {
        c_dec = self;
        c_num_items = num_items;
        c_offset = offset_after_n + size_num_items;
      }
    in
    idx_cstor, c
  | _ -> fail_decode_type_ ~what:"cstor" offset

let get_entrypoint (self : t) : offset =
  assert (Slice.len self.sl > 0);
  let last = Slice.len self.sl - 1 in
  let offset = Char.code @@ Slice.get self.sl last in
  last - offset - 1

let read_entrypoint (self : t) : Value.t =
  read self @@ deref_rec self @@ get_entrypoint self

let decode_string (d : _ decoder) (s : string) =
  let self = of_string s in
  let off = deref_rec self @@ get_entrypoint self in
  d self off

module Array_cursor = struct
  type t = cursor [@@deriving show]

  let[@inline] length (self : t) = self.c_num_items
  let[@inline] current self = self.c_offset

  let consume self =
    if self.c_num_items <= 0 then fail "Twine: cursor: no more elements";
    self.c_offset <- self.c_offset + skip_ self.c_dec self.c_offset;
    self.c_num_items <- self.c_num_items - 1

  let get_value_and_consume (self : t) : Value.t =
    if self.c_num_items <= 0 then fail "Twine: cursor: no more elements";
    let v = read self.c_dec self.c_offset in
    consume self;
    v

  let to_iter self yield =
    while length self > 0 do
      yield (get_value_and_consume self)
    done

  let to_array_of f self =
    Array.init (length self) (fun _ ->
        let x = current self in
        consume self;
        f x)

  let to_list self = to_iter self |> Iter.to_list

  let to_list_of f self =
    List.init (length self) (fun _ ->
        let x = current self in
        consume self;
        f x)
end

module Dict_cursor = struct
  type t = cursor [@@deriving show]

  let[@inline] length (self : t) = self.c_num_items

  let current self =
    let off = self.c_offset in
    off, off + skip_ self.c_dec off

  let consume self =
    if self.c_num_items <= 0 then fail "Twine: cursor: no more elements";

    (* skip the key and value *)
    let off = self.c_offset in
    let off = off + skip_ self.c_dec off in
    let off = off + skip_ self.c_dec off in
    self.c_offset <- off;

    self.c_num_items <- self.c_num_items - 1

  let get_key_value_and_consume (self : t) : Value.t * Value.t =
    if self.c_num_items <= 0 then fail "Twine: cursor: no more elements";

    let k, v = current self in
    let k = read self.c_dec k in
    let v = read self.c_dec v in
    consume self;
    k, v

  let to_iter self yield =
    while length self > 0 do
      yield (get_key_value_and_consume self)
    done

  let to_array_of f self =
    Array.init (length self) (fun _ ->
        let k, v = current self in
        consume self;
        f k v)

  let to_list self = to_iter self |> Iter.to_list

  let to_list_of f self =
    List.init (length self) (fun _ ->
        let k, v = current self in
        consume self;
        f k v)
end

type 'a cache_key = (module CACHE_KEY with type elt = 'a)

let create_cache_key (type a) () : a cache_key =
  (module struct
    type elt = a
    type cached += C of a
  end)

let with_cache (type a) (key : a cache_key) (dec : a decoder) : a decoder =
 fun st c ->
  let (module K) = key in
  (* make sure we use the canonical offset *)
  let c = deref_rec st c in
  match Offset_tbl.find_opt st.cache c with
  | Some (K.C v) -> v
  | Some _ -> dec st c
  | None ->
    let v = dec st c in
    Offset_tbl.add st.cache c (K.C v);
    v
