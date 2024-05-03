open Types

type decoder = {
  sl: Slice.t;
  records: record_descriptor Int_tbl.t;
  sum_types: sum_type_descriptor Int_tbl.t;
}

let create sl : decoder =
  { sl; records = Int_tbl.create 8; sum_types = Int_tbl.create 8 }

type 'a t = decoder -> offset -> 'a

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

let[@inline] check_bound_ (self : decoder) off len =
  if off + len >= self.sl.len then
    failf "Twine: bound check error (offset=%d, len=%d)" off len

let u8_to_int = Char.code

let[@inline never] bad_tag ~expected off c =
  failf "Twine: expected %s, got tag %C at offset %d" expected c off

let[@inline] small_int_ (self : decoder) off c : int =
  match c with
  | 'i' ->
    check_bound_ self off 1;
    u8_to_int (Slice.get self.sl (off + 1))
  | 'I' ->
    check_bound_ self off 2;
    Bytes.get_int16_le self.sl.bs (off + 1)
  | 'l' ->
    check_bound_ self off 5;
    Bytes.get_int32_le self.sl.bs (off + 1) |> Int32.to_int
  | _ -> assert false

let int_truncate (self : decoder) off =
  match Slice.get self.sl off with
  | ('i' | 'I' | 'l') as c -> small_int_ self off c
  | 'L' ->
    check_bound_ self off 9;
    Bytes.get_int64_le self.sl.bs (off + 1) |> Int64.to_int
  | c -> bad_tag ~expected:"int" off c

let int64 (self : decoder) off : int64 =
  match Slice.get self.sl off with
  | ('i' | 'I' | 'l') as c -> small_int_ self off c |> Int64.of_int
  | 'L' ->
    check_bound_ self off 9;
    Bytes.get_int64_le self.sl.bs (off + 1)
  | c -> bad_tag ~expected:"int" off c

let pointer_ (self : decoder) off c =
  match c with
  | 'p' ->
    check_bound_ self off 3;
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

let rec skip (self:decoder) (off:offset) : offset =
  match Slice.get self.sl off with
  | 'T' | 'F' | 'N' -> off+1
  | 'i' -> off + 2
  | 'I' -> off + 3
  | 'l' -> off + 5
  | 'L' -> off + 9
  | 'd' -> off + 5
  | 'D' -> off + 9
  | 's' | 'b' ->
      let len = u8_to_int (Slice.get self.sl (off+1)) in
      off + 2 + len
  | 'S' | 'B' ->
      let len = int_truncate self (off+1) in
      (* skip the length *)
      skip self (off+1) + len
  | 'K' -> skip self (off+1)
  | 'p' -> off +3
  | 'P' -> skip self (off+1)
  | '{' -> Dict
  | '[' -> Array
  | 'R' -> Record
  | 'C' -> Constructor

let float (self : decoder) off : float =
  match Slice.get self.sl off with
  | 'f' ->
    check_bound_ self off 5;
    let i32 = Bytes.get_int32_le self.sl.bs (off + 1) in
    Int32.float_of_bits i32
  | 'F' ->
    check_bound_ self off 9;
    let i64 = Bytes.get_int64_le self.sl.bs (off + 1) in
    Int64.float_of_bits i64
  | c -> bad_tag ~expected:"float" off c

let key_content (self : decoder) off =
  match Slice.get self.sl off with
  | 'K' ->
    check_bound_ self off 1;
    off + 1
  | c -> bad_tag ~expected:"key" off c

let array (self:decoder) off st f : unit =
  match Slice.get self.sl off with
  | '[' ->
    let off = ref (off+1) in
    while Slice.get self.sl off != ']' do
      

    done;

