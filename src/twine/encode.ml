open Types

type encoder = {
  mutable buf: bytes;
  mutable offset: int;
  descriptors: offset Str_tbl.t;
      (** Record/sum type descriptor -> their offset *)
}

let create () : encoder =
  { buf = Bytes.create 256; offset = 0; descriptors = Str_tbl.create 8 }

let[@inline] finalize (self : encoder) : Slice.t =
  Slice.create ~len:self.offset self.buf

type 'a t = encoder -> 'a -> offset
type 'a t' = encoder -> 'a -> unit

let ignore_offset : offset -> unit = ignore

(** How much space there is at the end of the buffer *)
let[@inline] free_space_ (self : encoder) : int =
  Bytes.length self.buf - self.offset

open struct
  let[@inline never] reserve_slow_ (self : encoder) n =
    let next_size =
      min Sys.max_string_length
        (max (free_space_ self * 2) (free_space_ self + n + 2))
    in
    if next_size < free_space_ self + n then fail "Twine: max length exceeded";
    let new_buf = Bytes.create next_size in
    Bytes.blit self.buf 0 new_buf 0 self.offset;
    self.buf <- new_buf

  let[@inline] reserve (self : encoder) (n : int) : offset =
    let free = free_space_ self in
    let off = self.offset in
    if free < n then reserve_slow_ self n;
    self.offset <- off + n;
    off

  (** Write a single byte *)
  let[@inline] write_b_ (self : encoder) c : offset =
    let off = reserve self 1 in
    Bytes.set self.buf off c;
    off

  let[@inline] write_string_ (self : encoder) s : offset =
    let len = String.length s in
    let off = reserve self len in
    Bytes.blit self.buf off (Bytes.unsafe_of_string s) 0 len;
    off
end

let[@inline] null (self : encoder) () = write_b_ self 'N'
let[@inline] key (self : encoder) () = write_b_ self 'K'

let[@inline] bool (self : encoder) (b : bool) =
  write_b_ self
    (if b then
      'T'
    else
      'F')

let max_u16 = (1 lsl 16) - 1
let max_i16 = (1 lsl 15) - 1
let min_i16 = -max_i16 - 1
let u8_to_char_ = Char.unsafe_chr

let[@inline] int64_full_ (self : encoder) (i : int64) : offset =
  let off = reserve self 9 in
  Bytes.set self.buf off 'L';
  Bytes.set_int64_le self.buf (off + 1) i;
  off

let int (self : encoder) (i : int) =
  if i >= 0 && i < 256 then (
    let off = reserve self 2 in
    Bytes.set self.buf off 'i';
    Bytes.set self.buf off (u8_to_char_ i);
    off
  ) else if i >= min_i16 && i <= max_i16 then (
    let off = reserve self 3 in
    Bytes.set self.buf off 'I';
    Bytes.set_int16_le self.buf (off + 1) i;
    off
  ) else if i = Int32.(to_int (of_int i)) then (
    let off = reserve self 5 in
    Bytes.set self.buf off 'l';
    Bytes.set_int32_le self.buf (off + 1) (Int32.of_int i);
    off
  ) else
    int64_full_ self (Int64.of_int i)

let pointer (self : encoder) (p : offset) =
  let off = self.offset in
  let delta = off - p in
  (* can only point to the past *)
  assert (delta > 0);
  if delta < max_u16 then (
    (* relative pointer *)
    let off = reserve self 3 in
    Bytes.set self.buf off 'p';
    Bytes.set_uint16_le self.buf (off + 1) delta;
    off
  ) else (
    (* absolute pointer *)
    let off = write_b_ self 'P' in
    int self p |> ignore_offset;
    off
  )

let int64 (self : encoder) (i : int64) =
  if i < Int64.of_int min_int || i > Int64.of_int max_int then (
    let off = reserve self 9 in
    Bytes.set self.buf off 'L';
    Bytes.set_int64_le self.buf (off + 1) i;
    off
  ) else
    int self (Int64.to_int i)

let float32 (self : encoder) (f : float) =
  let off = reserve self 5 in
  Bytes.set self.buf off 'd';
  let as_i32 = Int32.bits_of_float f in
  Bytes.set_int32_le self.buf (off + 1) as_i32;
  off

let float (self : encoder) (f : float) =
  let off = reserve self 9 in
  Bytes.set self.buf off 'D';
  let as_i64 = Int64.bits_of_float f in
  Bytes.set_int64_le self.buf (off + 1) as_i64;
  off

let string (self : encoder) (s : string) =
  let len = String.length s in
  if len < 256 then (
    let off = reserve self (2 + len) in
    Bytes.set self.buf off 's';
    Bytes.set self.buf (off + 1) (u8_to_char_ len);
    Bytes.blit (Bytes.unsafe_of_string s) 0 self.buf (off + 2) len;
    off
  ) else (
    let off = write_b_ self 'S' in
    int self len |> ignore_offset;
    write_string_ self s |> ignore_offset;
    off
  )

let blob (self : encoder) (s : string) =
  let len = String.length s in
  if len < 256 then (
    let off = reserve self (2 + len) in
    Bytes.set self.buf off 'b';
    Bytes.set self.buf (off + 1) (u8_to_char_ len);
    Bytes.blit (Bytes.unsafe_of_string s) 0 self.buf (off + 2) len;
    off
  ) else (
    let off = write_b_ self 'B' in
    int self len |> ignore_offset;
    write_string_ self s |> ignore_offset;
    off
  )

let[@inline] array_start (self : encoder) () = write_b_ self '['

let[@inline] array_start' (self : encoder) () =
  array_start self () |> ignore_offset

let[@inline] array_stop (self : encoder) () = write_b_ self ']' |> ignore_offset
let[@inline] dict_start (self : encoder) () = write_b_ self '{'

let[@inline] dict_start' (self : encoder) () =
  dict_start self () |> ignore_offset

let[@inline] dict_stop (self : encoder) () = write_b_ self '}' |> ignore_offset

(* write "{fs: [name, name…]}" *)
let record_descriptor_ (self : encoder) (r : record_descriptor) : offset =
  let off = dict_start self () in
  string self "fs" |> ignore_offset;
  array_start self () |> ignore_offset;
  Array.iter (fun name -> string self name |> ignore_offset) r.fs;
  array_stop self ();
  dict_stop self ();
  off

let record_descriptor (self : encoder) ~key ~f () : offset =
  match Str_tbl.find_opt self.descriptors key with
  | Some p -> p
  | None ->
    let descr = f () in
    let off = record_descriptor_ self descr in
    Str_tbl.add self.descriptors key off;
    off

(* write "{cs: [{ar: <n>}, …]}" *)
let cstor_descriptor_ (self : encoder) (r : cstor_descriptor) : offset =
  let off = dict_start self () in
  string self "ar" |> ignore_offset;
  int self r.ar |> ignore_offset;
  Option.iter
    (fun names ->
      assert (Array.length names = r.ar);
      string self "fs" |> ignore_offset;
      array_start' self ();
      Array.iter (fun name -> string self name |> ignore_offset) names;
      array_stop self ())
    r.fs;
  dict_stop self ();
  off

(* write "{cs: [{ar: <n>}, …]}" *)
let sum_type_descriptor_ (self : encoder) (r : sum_type_descriptor) : offset =
  let off = dict_start self () in
  string self "cs" |> ignore_offset;
  array_start' self ();
  Array.iter (fun c -> cstor_descriptor_ self c |> ignore_offset) r.cs;
  array_stop self ();
  dict_stop self ();
  off

let record_descriptor (self : encoder) ~key ~f () : offset =
  match Str_tbl.find_opt self.descriptors key with
  | Some p -> p
  | None ->
    let descr = f () in
    let off = record_descriptor_ self descr in
    Str_tbl.add self.descriptors key off;
    off

let sum_type_descriptor (self : encoder) ~key ~f () : offset =
  match Str_tbl.find_opt self.descriptors key with
  | Some p -> p
  | None ->
    let descr = f () in
    let off = sum_type_descriptor_ self descr in
    Str_tbl.add self.descriptors key off;
    off

let record (self : encoder) ~descr () : offset =
  let off = write_b_ self 'R' in
  int self descr |> ignore_offset;
  off

let cstor (self : encoder) ~descr ~index () =
  assert (index < 256);
  let off = reserve self 2 in
  Bytes.set self.buf off 'C';
  Bytes.set self.buf (off + 1) (u8_to_char_ index);
  int self descr |> ignore_offset;
  off
