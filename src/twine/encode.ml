open Types
module Immediate = Immediate
module Buf = Byte_buf
module LEB128 = Imandrakit_leb128
module Slice = Byte_slice

module type CACHE_KEY = sig
  include Hashtbl.HashedType

  val id : int
end

type immediate = Immediate.t
type 'a cache_key = (module CACHE_KEY with type t = 'a)
type cache_key_with_val = K : 'a cache_key * 'a -> cache_key_with_val

module Cache_tbl = Hashtbl.Make (struct
  type t = cache_key_with_val

  let equal (K ((module C1), v1)) (K ((module C2), v2)) : bool =
    C1.id = C2.id && C1.equal v1 (Obj.magic v2)

  let hash (K ((module C), v)) = C.hash v
end)

type t = {
  buf: Buf.t;
  cache: immediate Cache_tbl.t;
}

let create () : t = { buf = Buf.create ~cap:256 (); cache = Cache_tbl.create 8 }
let reset self = Buf.clear self.buf

type 'a encoder = t -> 'a -> immediate

let ignore_offset : offset -> unit = ignore

(** [reserve_space_ self n] reserves [n] bytes at the end of the
    buffer, bumps the buffer by [n] bytes, and returns the offset
    to the beginning of the newly reserved area. *)
let[@inline] reserve_space_ (self : t) (n : int) =
  let off = self.buf.len in
  Buf.ensure_free self.buf n;
  self.buf.len <- self.buf.len + n;
  off

let u8_to_char_ = Char.unsafe_chr

external int_of_bool : bool -> int = "%identity"

(** Write the first byte. See [decode.ml] for the list *)
let[@inline] first_byte_ ~high ~low : char = u8_to_char_ ((high lsl 4) lor low)

(** Write a single byte value *)
let[@inline] write_first_byte_ (self : t) ~high ~low : offset =
  let off = reserve_space_ self 1 in
  Bytes.set self.buf.bs off (first_byte_ ~high ~low);
  off

let write_first_byte_and_int (self : t) ~high ~(n : int) : offset =
  assert (n >= 0);
  if n < 15 then
    write_first_byte_ self ~high ~low:n
  else (
    let off = write_first_byte_ self ~high ~low:15 in
    LEB128.Encode.uint self.buf (n - 15);
    off
  )

(** Write the first byte, followed by an integer (possibly fitting in [low]) *)
let write_first_byte_and_int64 (self : t) ~high ~(n : int64) : offset =
  if Int64.(equal (of_int (to_int n)) n) then
    write_first_byte_and_int self ~high ~n:(Int64.to_int n)
  else (
    assert (Int64.compare n 0L >= 0);
    let off = write_first_byte_ self ~high ~low:15 in
    LEB128.Encode.u64 self.buf Int64.(sub n 15L);
    off
  )

open struct
  let[@inline] write_null (self : t) () = write_first_byte_ self ~high:0 ~low:2

  let[@inline] write_bool (self : t) b =
    write_first_byte_ self ~high:0 ~low:(int_of_bool b)

  let[@inline] write_int64 (self : t) n : offset =
    if Int64.(compare n 0L < 0) then
      write_first_byte_and_int64 self ~high:2 ~n:Int64.(sub (neg n) 1L)
    else
      write_first_byte_and_int64 self ~high:1 ~n

  let[@inline] write_pointer (self : t) (p : offset) =
    let off = self.buf.len in
    assert (off > p);
    (* compute relative offset to [p] *)
    let n = off - p - 1 in
    ignore_offset (write_first_byte_and_int self ~high:15 ~n);
    off

  let write_float32 (self : t) (f : float) =
    let off = reserve_space_ self 5 in
    Bytes.set self.buf.bs off (first_byte_ ~high:3 ~low:0);
    let as_i32 = Int32.bits_of_float f in
    Bytes.set_int32_le self.buf.bs (off + 1) as_i32;
    off

  let write_float (self : t) (f : float) =
    let off = reserve_space_ self 9 in
    Bytes.set self.buf.bs off (first_byte_ ~high:3 ~low:1);
    let as_i64 = Int64.bits_of_float f in
    Bytes.set_int64_le self.buf.bs (off + 1) as_i64;
    off

  let write_string_slice (self : t) (s : slice) =
    let len = Slice.len s in
    let off = write_first_byte_and_int self ~high:4 ~n:len in
    let off_str = reserve_space_ self len in
    Bytes.blit s.bs s.off self.buf.bs off_str len;
    off

  let write_blob_slice (self : t) (s : slice) =
    let len = Slice.len s in
    let off = write_first_byte_and_int self ~high:5 ~n:len in
    let off_str = reserve_space_ self len in
    Bytes.blit s.bs s.off self.buf.bs off_str len;
    off

  let[@inline] write_cstor0 (self : t) ~index : offset =
    write_first_byte_and_int self ~high:10 ~n:index
end

let write_immediate (self : t) (v : immediate) : offset =
  match v with
  | Null -> write_null self ()
  | True -> write_bool self true
  | False -> write_bool self false
  | Int i -> write_int64 self i
  | Float32 f -> write_float32 self f
  | Float f -> write_float self f
  | String s -> write_string_slice self s
  | Blob s -> write_blob_slice self s
  | Pointer p -> write_pointer self p
  | Cstor0 index -> write_cstor0 self ~index

let write_or_ref_immediate (self : t) (v : immediate) : offset =
  match v with
  | Pointer p -> p
  | _ -> write_immediate self v

let tag (self : t) ~tag ~(v : immediate) : immediate =
  let off = write_first_byte_and_int self ~high:8 ~n:tag in
  write_immediate self v |> ignore_offset;
  Immediate.pointer off

let array (self : t) vs : immediate =
  let n = Array.length vs in
  let off = write_first_byte_and_int self ~high:6 ~n in
  for i = 0 to n - 1 do
    let v = Array.unsafe_get vs i in
    write_immediate self v |> ignore_offset
  done;
  Immediate.pointer off

let array_init (self : t) n f : immediate =
  (* first, make sure we get all the intermediates before writing them,
     as the act of turning real values into intermediate will do some
     writes (e.g. we write a complex record and get its offset, but we
     can't turn that into a pointer yet because writing the next
     record will change where the pointer is written). So we get all
     the intermediates and then write them without interference. *)
  let vs = Array.init n f in
  array self vs

let list (self : t) vs : immediate =
  let off = write_first_byte_and_int self ~high:6 ~n:(List.length vs) in
  List.iter (fun v -> ignore_offset @@ write_immediate self v) vs;
  Immediate.pointer off

let array_iter (self : t) iter : immediate = list self @@ Iter.to_list iter

let dict (self : t) n f : immediate =
  let pairs : (immediate * immediate) array = Array.init n f in
  let off = write_first_byte_and_int self ~high:7 ~n in
  for i = 0 to n - 1 do
    let k, v = Array.unsafe_get pairs i in
    ignore_offset @@ write_immediate self k;
    ignore_offset @@ write_immediate self v
  done;
  Immediate.pointer off

let dict_list (self : t) pairs : immediate =
  let off = write_first_byte_and_int self ~high:7 ~n:(List.length pairs) in
  List.iter
    (fun (k, v) ->
      ignore_offset @@ write_immediate self k;
      ignore_offset @@ write_immediate self v)
    pairs;
  Immediate.pointer off

let dict_iter (self : t) pairs : immediate =
  dict_list self @@ Iter.to_list pairs

let cstor (self : t) ~(index : int) (args : immediate array) : immediate =
  match Array.length args with
  | 0 -> Immediate.cstor0 ~index
  | 1 ->
    let off = write_first_byte_and_int self ~high:11 ~n:index in
    ignore_offset @@ write_immediate self args.(0);
    Immediate.pointer off
  | _ ->
    let off = write_first_byte_and_int self ~high:12 ~n:index in
    (* now write number of arguments *)
    LEB128.Encode.uint self.buf (Array.length args);
    Array.iter (fun v -> ignore_offset @@ write_immediate self v) args;
    Immediate.pointer off

let finalize (self : t) ~(entrypoint : immediate) : slice =
  let top = write_or_ref_immediate self entrypoint in
  assert (top < self.buf.len);

  let rec finalize_offset top =
    let delta = self.buf.len - top - 1 in
    if delta > 250 then (
      (* go through intermediate pointer (uncommon, can happen if last value is ginormous) *)
      let ptr_to_top = write_pointer self top in
      finalize_offset ptr_to_top
    ) else (
      Buf.add_char self.buf (u8_to_char_ delta);
      Buf.to_slice self.buf
    )
  in
  finalize_offset top

let[@inline] finalize_copy self ~entrypoint : string =
  Slice.contents @@ finalize self ~entrypoint

let encode_to_string (e : _ encoder) x : string =
  let enc = create () in
  let entrypoint = e enc x in
  finalize_copy enc ~entrypoint

let to_string = encode_to_string

open struct
  let cache_id_ = Atomic.make 0
end

let create_cache_key (type a) (module H : Hashtbl.HashedType with type t = a) :
    a cache_key =
  let id = Atomic.fetch_and_add cache_id_ 1 in
  (module struct
    include H

    let id = id
  end)

let default_string_cache_threshold = 20

let with_cache ?(max_string_size = default_string_cache_threshold)
    (key : 'a cache_key) (enc : 'a encoder) : 'a encoder =
 fun st (x : 'a) : immediate ->
  let k = K (key, x) in
  match Cache_tbl.find_opt st.cache k with
  | Some v -> v
  | None ->
    (* encode and save the pointer *)
    let v = enc st x in
    let v =
      match v with
      | (Immediate.String s | Immediate.Blob s)
        when Slice.len s > max_string_size ->
        (* for large strings, write them once and point to them *)
        Immediate.pointer @@ write_immediate st v
      | _ -> v
    in

    Cache_tbl.add st.cache k v;
    v

let add_cache ?max_string_size h (enc_ref : _ encoder ref) : unit =
  let key = create_cache_key h in
  enc_ref := with_cache ?max_string_size key !enc_ref

let add_cache_with (type t) ?max_string_size ~eq ~hash enc_ref =
  let module M = struct
    type nonrec t = t

    let equal = eq
    let hash = hash
  end in
  add_cache ?max_string_size (module M) enc_ref
