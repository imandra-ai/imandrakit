open Types
module Immediate = Immediate
module Buf = Byte_buf
module LEB128 = Imandrakit_leb128
module Slice = Byte_slice

type t = { buf: Buf.t } [@@unboxed]

let create () : t = { buf = Buf.create ~cap:256 () }
let reset self = Buf.clear self.buf

type 'a encoder = t -> 'a -> offset

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

let[@inline] null (self : t) () = write_first_byte_ self ~high:0 ~low:2

let[@inline] bool (self : t) b =
  write_first_byte_ self ~high:0 ~low:(int_of_bool b)

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

let[@inline] int (self : t) n : offset =
  if n < 0 then
    write_first_byte_and_int self ~high:2 ~n:(-n - 1)
  else
    write_first_byte_and_int self ~high:1 ~n

let[@inline] int64 (self : t) n : offset =
  if Int64.(compare n 0L < 0) then
    write_first_byte_and_int64 self ~high:2 ~n:Int64.(sub (neg n) 1L)
  else
    write_first_byte_and_int64 self ~high:1 ~n

let[@inline] pointer (self : t) (p : offset) =
  let off = self.buf.len in
  assert (off > p);
  (* compute relative offset to [p] *)
  let n = off - p - 1 in
  ignore_offset (write_first_byte_and_int self ~high:15 ~n);
  off

let float32 (self : t) (f : float) =
  let off = reserve_space_ self 5 in
  Bytes.set self.buf.bs off (first_byte_ ~high:3 ~low:0);
  let as_i32 = Int32.bits_of_float f in
  Bytes.set_int32_le self.buf.bs (off + 1) as_i32;
  off

let float (self : t) (f : float) =
  let off = reserve_space_ self 9 in
  Bytes.set self.buf.bs off (first_byte_ ~high:3 ~low:1);
  let as_i64 = Int64.bits_of_float f in
  Bytes.set_int64_le self.buf.bs (off + 1) as_i64;
  off

let string_slice (self : t) (s : slice) =
  let len = Slice.len s in
  let off = write_first_byte_and_int self ~high:4 ~n:len in
  let off_str = reserve_space_ self len in
  Bytes.blit s.bs s.off self.buf.bs off_str len;
  off

let[@inline] string (self : t) (s : string) =
  string_slice self (Slice.of_string s)

let blob_slice (self : t) (s : slice) =
  let len = Slice.len s in
  let off = write_first_byte_and_int self ~high:5 ~n:len in
  let off_str = reserve_space_ self len in
  Bytes.blit s.bs s.off self.buf.bs off_str len;
  off

let[@inline] blob self s = blob_slice self (Slice.of_string s)

let tag (self : t) ~tag ~v : offset =
  let off = write_first_byte_and_int self ~high:8 ~n:tag in
  LEB128.Encode.int self.buf v;
  off

let cstor0 (self : t) ~index : offset =
  write_first_byte_and_int self ~high:10 ~n:index

let immediate (self : t) (v : immediate) : offset =
  match v with
  | Null -> null self ()
  | True -> bool self true
  | False -> bool self false
  | Int i -> int64 self i
  | Float f -> float self f
  | String s -> string_slice self s
  | Blob s -> blob_slice self s
  | Pointer p -> pointer self p
  | Cstor0 index -> cstor0 self ~index

let array (self : t) vs : offset =
  let n = Array.length vs in
  let off = write_first_byte_and_int self ~high:6 ~n in
  for i = 0 to n - 1 do
    let v = Array.unsafe_get vs i in
    ignore_offset @@ immediate self v
  done;
  off

let array_init (self : t) n f : offset =
  (* first, make sure we get all the intermediates before writing them,
     as the act of turning real values into intermediate will do some
     writes (e.g. we write a complex record and get its offset, but we
     can't turn that into a pointer yet because writing the next
     record will change where the pointer is written). So we get all
     the intermediates and then write them without interference. *)
  let vs = Array.init n f in
  array self vs

let list (self : t) vs : offset =
  let off = write_first_byte_and_int self ~high:6 ~n:(List.length vs) in
  List.iter (fun v -> ignore_offset @@ immediate self v) vs;
  off

let array_iter (self : t) iter : offset = list self @@ Iter.to_list iter

let dict (self : t) n f : offset =
  let pairs : (immediate * immediate) array = Array.init n f in
  let off = write_first_byte_and_int self ~high:7 ~n in
  for i = 0 to n - 1 do
    let k, v = Array.unsafe_get pairs i in
    ignore_offset @@ immediate self k;
    ignore_offset @@ immediate self v
  done;
  off

let dict_list (self : t) pairs : offset =
  let off = write_first_byte_and_int self ~high:7 ~n:(List.length pairs) in
  List.iter
    (fun (k, v) ->
      ignore_offset @@ immediate self k;
      ignore_offset @@ immediate self v)
    pairs;
  off

let dict_iter (self : t) pairs : offset = dict_list self @@ Iter.to_list pairs

let cstor (self : t) ~(index : int) (args : immediate array) =
  match Array.length args with
  | 0 -> cstor0 self ~index
  | 1 ->
    let off = write_first_byte_and_int self ~high:11 ~n:index in
    ignore_offset @@ immediate self args.(0);
    off
  | _ ->
    let off = write_first_byte_and_int self ~high:12 ~n:index in
    (* now write number of arguments *)
    LEB128.Encode.uint self.buf (Array.length args);
    Array.iter (fun v -> ignore_offset @@ immediate self v) args;
    off

let rec finalize (self : t) ~top : slice =
  assert (top < self.buf.len);
  let delta = self.buf.len - top - 1 in
  if delta > 250 then (
    (* go through intermediate pointer (uncommon, can happen if last value is ginormous) *)
    let ptr_to_top = pointer self top in
    finalize self ~top:ptr_to_top
  ) else (
    Buf.add_char self.buf (u8_to_char_ delta);
    Buf.to_slice self.buf
  )

let[@inline] finalize_copy self ~top : string =
  Slice.contents @@ finalize self ~top

let to_string (e : _ encoder) x : string =
  let enc = create () in
  let off = e enc x in
  finalize_copy enc ~top:off
