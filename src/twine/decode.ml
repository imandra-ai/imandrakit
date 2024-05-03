open Types

type decoder = {
  sl: Slice.t;
  records: record_descriptor Int_tbl.t;
  sum_types: sum_type_descriptor Int_tbl.t;
  mutable cur_offset: int;  (** Offset past the last read value *)
}

let create sl : decoder =
  {
    sl;
    records = Int_tbl.create 8;
    sum_types = Int_tbl.create 8;
    cur_offset = 0;
  }

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
