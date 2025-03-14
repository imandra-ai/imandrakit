open Types
open Common_
module Slice = Byte_slice
module LEB128 = Imandrakit_leb128.Decode

type cached = ..

module type CACHE_KEY = sig
  type elt
  type cached += C of elt
end

type t =
  | Decode : {
      st: 'st;
      ops: 'st ops;
      cache: cached Int_tbl.t;
      mutable hmap: Hmap.t;
    }
      -> t

and 'st ops = {
  length: 'st -> int;
  read_char: 'st -> int -> char;
  read_int32: 'st -> int -> int32;
  read_int64: 'st -> int -> int64;
  read_blob: 'st -> int -> int -> slice;
  read_leb128: 'st -> int -> int64 * offset;
}

let st_len t =
  let (Decode { st; ops; _ }) = t in
  ops.length st

let read_char t offset =
  let (Decode { st; ops; _ }) = t in
  ops.read_char st offset

let read_int32 t offset =
  let (Decode { st; ops; _ }) = t in
  ops.read_int32 st offset

let read_int64 t offset =
  let (Decode { st; ops; _ }) = t in
  ops.read_int64 st offset

let read_blob t offset length =
  let (Decode { st; ops; _ }) = t in
  ops.read_blob st offset length

let read_leb128 t offset =
  let (Decode { st; ops; _ }) = t in
  ops.read_leb128 st offset

let slice_ops : slice ops =
  {
    length = (fun (st : slice) -> Slice.len st);
    read_char = (fun (st : slice) (offset : int) -> Slice.get st offset);
    read_int32 =
      (fun (st : slice) (offset : int) ->
        Bytes.get_int32_le st.bs (st.off + offset));
    read_int64 =
      (fun (st : slice) (offset : int) ->
        Bytes.get_int64_le st.bs (st.off + offset));
    read_blob =
      (fun (st : slice) (offset : int) (length : int) ->
        Slice.sub st offset (min length (Slice.len st - st.off - offset)));
    read_leb128 = (fun (st : slice) (offset : int) -> LEB128.u64 st offset);
  }

let in_channel_ops : in_channel ops =
  {
    length = (fun (st : in_channel) -> Int64.to_int (In_channel.length st));
    read_char =
      (fun (st : in_channel) (offset : int) ->
        In_channel.seek st (Int64.of_int offset);
        Option.get (In_channel.input_char st));
    read_int32 =
      (fun (st : in_channel) (offset : int) ->
        let bs = Bytes.create 4 in
        In_channel.seek st (Int64.of_int offset);
        let _ = In_channel.input st bs 0 4 in
        Bytes.get_int32_le bs 0);
    read_int64 =
      (fun (st : in_channel) (offset : int) ->
        let bs = Bytes.create 8 in
        In_channel.seek st (Int64.of_int offset);
        let _ = In_channel.input st bs 0 8 in
        Bytes.get_int64_le bs 0);
    read_blob =
      (fun (st : in_channel) (offset : int) (length : int) ->
        let remaining = Int64.to_int (In_channel.length st) - offset in
        let length = min length remaining in
        let bs = Bytes.create length in
        In_channel.seek st (Int64.of_int offset);
        let _ = In_channel.input st bs 0 length in
        Slice.create bs);
    read_leb128 =
      (fun (st : in_channel) (offset : int) ->
        let remaining = Int64.to_int (In_channel.length st) - offset in
        let length = min 16 remaining in
        let bs = Bytes.create length in
        In_channel.seek st (Int64.of_int offset);
        let _ = In_channel.input st bs 0 length in
        LEB128.u64 (Slice.create bs) 0);
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

let[@inline] create (st : 'st) (ops : 'st ops) : t =
  Decode { st; ops; cache = Int_tbl.create 32; hmap = Hmap.empty }

let[@inline] of_slice s : t = create s slice_ops
let[@inline] of_string s : t = create (Slice.of_string s) slice_ops
let[@inline] of_in_channel c : t = create c in_channel_ops

let[@inline] hmap_set self k v =
  match self with
  | Decode d -> d.hmap <- Hmap.add k v d.hmap

let[@inline] hmap_get self k =
  match self with
  | Decode d -> Hmap.find k d.hmap

let hmap_transfer d1 ~into:d2 : unit =
  match d1, d2 with
  | Decode d1, Decode d2 ->
    d2.hmap <-
      Hmap.fold (fun (Hmap.B (k, v)) h2 -> Hmap.add k v h2) d1.hmap d2.hmap

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
    | Ref of offset
    | Pointer of offset
    | Array of array_cursor
    | Dict of dict_cursor
    | Tag of int * offset
    | Cstor0 of cstor_index
    | CstorN of cstor_index * array_cursor
  [@@deriving show { with_path = false }]
end

let fail = fail
let failf = failf

let invalid_first_byte_ msg ~offset ~high ~low =
  failf "Decode: invalid first byte %d/%d at %d: %s" high low offset msg

let[@inline] get_char_ (self : t) (offset : offset) : int =
  Char.code @@ read_char self offset

let[@inline] get_high (c : int) : int = (c land 0b1111_0000) lsr 4
let[@inline] get_low (c : int) : int = c land 0b0000_1111

(** Read a int64 value starting at [offset] *)
let[@inline] get_int64_ self offset ~low : int64 * num_bytes_consumed =
  if low < 15 then
    Int64.of_int low, 0
  else (
    let i, n = read_leb128 self (offset + 1) in
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
    read_int32 self (offset + 1) |> Int32.float_of_bits, 4
  else
    read_int64 self (offset + 1) |> Int64.float_of_bits, 8

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

let read ?(auto_deref = true) (self : t) (offset : offset) : Value.t =
  let offset =
    if auto_deref then
      deref_rec self offset
    else
      offset
  in
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
    let s = read_blob self (offset + 1 + size_len) len in
    Value.String s
  | 5 ->
    let len, size_len = get_int_truncate_ self offset ~low in
    let s = read_blob self (offset + 1 + size_len) len in
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
    let c : cursor =
      { c_dec = self; c_num_items = 1; c_offset = offset + 1 + size_idx_cstor }
    in
    Value.CstorN (idx_cstor, c)
  | 12 ->
    let idx_cstor, size_idx_cstor = get_int_truncate_ self offset ~low in
    let offset_after_n = offset + 1 + size_idx_cstor in
    let num_items, size_num_items =
      let sl = read_blob self offset_after_n 16 in
      LEB128.uint_truncate sl 0
    in
    let c : cursor =
      {
        c_dec = self;
        c_num_items = num_items;
        c_offset = offset_after_n + size_num_items;
      }
    in
    Value.CstorN (idx_cstor, c)
  | 13 -> invalid_first_byte_ ~offset ~high ~low "type is reserved"
  | 14 ->
    let n, _ = get_int_truncate_ self offset ~low in
    Value.Ref (offset - n - 1)
  | 15 ->
    let n, _ = get_int_truncate_ self offset ~low in
    Value.Pointer (offset - n - 1)
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
  | 13 -> invalid_first_byte_ ~offset ~high ~low "type is reserved"
  | 14 | 15 ->
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

let[@inline] int64 self offset =
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
    (* Printf.eprintf "high=%d, low=%d\n%!" high low; *)
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
  read_blob self (offset + 1 + size_len) len

let string self offset = Slice.contents @@ string_slice self offset

let blob_slice self offset =
  let offset = deref_rec self offset in
  let c = get_char_ self offset in
  let high = get_high c in
  let low = get_low c in
  if high <> 5 then fail_decode_type_ ~what:"blob" offset;
  let len, size_len = get_int_truncate_ self offset ~low in
  read_blob self (offset + 1 + size_len) len

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
      let sl = read_blob self offset_after_n 16 in
      LEB128.uint_truncate sl 0
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

let ref_ (self : t) offset : offset =
  let offset = deref_rec self offset in
  let c = get_char_ self offset in
  let high = get_high c in
  if high <> 14 then fail_decode_type_ ~what:"ref" offset;
  let low = get_low c in
  let p, _ = get_int_truncate_ self offset ~low in
  offset - p - 1

let[@inline] ref_for (self : t) offset : _ offset_for =
  Offset_for (ref_ self offset)

let get_entrypoint (self : t) : offset =
  let len = st_len self in
  assert (len > 0);
  let last = len - 1 in
  let offset = Char.code @@ read_char self last in
  last - offset - 1

let read_entrypoint (self : t) : Value.t =
  read self @@ deref_rec self @@ get_entrypoint self

let decode_string ?(init = ignore) (d : 'a decoder) (s : string) : 'a =
  let self = of_string s in
  init self;
  let off = deref_rec self @@ get_entrypoint self in
  d self off

let[@inline] read_ref (self : t) (d : 'a decoder)
    (Offset_for offset : 'a offset_for) : 'a =
  d self offset

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

  let to_iter_of self f yield =
    while length self > 0 do
      let x = f (current self) in
      consume self;
      yield x
    done

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

  let to_iter_of self f yield =
    while length self > 0 do
      let k, v = current self in
      let res = f k v in
      consume self;
      yield res
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
 fun st off ->
  let (module K) = key in
  (* make sure we use the canonical offset *)
  let off = deref_rec st off in
  let c = get_char_ st off in
  let high = get_high c in
  if high < 6 || high = 10 then
    (* easy immediate value *)
    dec st off
  else (
    (* go through the cache *)
    let (Decode { cache; _ }) = st in
    match Int_tbl.find cache off with
    | exception Not_found ->
      let v = dec st off in
      Int_tbl.add cache off (K.C v);
      v
    | K.C v -> v
    | _ -> (* weird collision, just don't cache… *) dec st off
  )

let add_cache (dec_ref : _ decoder ref) : unit =
  let key = create_cache_key () in
  dec_ref := with_cache key !dec_ref
