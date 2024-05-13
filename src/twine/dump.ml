open Types

open struct
  module Slice = Byte_slice

  let bpf = Printf.bprintf
end

let hex_of_string = CCString.to_hex

(** Does [s] print ok with "%S"? *)
let string_is_printable (s : string) : bool =
  try
    let[@inline] is_ok = function
      | '\x07' .. '\x0d' -> true
      | '\x20' .. '\x7e' -> true
      | _ -> false
    in
    String.iter (fun c -> if not (is_ok c) then raise Exit) s;
    true
  with Exit -> false

let deref_rec = Decode.deref_rec

let dump_bytes_summary b : string =
  let b, tail =
    if String.length b > 20 then
      String.sub b 0 20, spf "…[%dB omitted]" (String.length b - 20)
    else
      b, ""
  in
  if string_is_printable b then
    spf "bytes(%S%s)" b tail
  else
    spf "bytes(h'%s%s')" (hex_of_string b) tail

(** Print primitive and a quick view of composite values *)
let rec dump_primitive (self : Decode.t) (v : Decode.Value.t) =
  match v with
  | Null -> "null"
  | True -> "true"
  | False -> "false"
  | Int i -> spf "%Ld" i
  | Float f -> spf "%f" f
  | String s ->
    let s = Slice.contents s in
    if String.length s > 20 then
      spf "%S[…%d omitted]" (String.sub s 0 20) (String.length s - 20)
    else
      spf "%S" s
  | Blob b -> dump_bytes_summary @@ Slice.contents b
  | Pointer p -> spf "@0x%x" p
  | Tag (n, v) -> spf "%d(@0x%x)" n v
  | Array c -> spf "[…[%d omitted]]" (Decode.Array_cursor.length c)
  | Dict c -> spf "{…[%d omitted]}" (Decode.Dict_cursor.length c)
  | Cstor0 idx -> spf "C_%d" idx
  | Cstor1 (idx, p) ->
    let a = Decode.read self @@ deref_rec self p in
    spf "C_%d(%s)" idx (dump_primitive self a)
  | CstorN (idx, c) ->
    spf "C_%d(%s)" idx
      (String.concat ","
      @@ List.map (dump_primitive self)
      @@ Decode.Array_cursor.to_list c)

type state = {
  dec: Decode.t;
  mutable offset: string Int_map.t;  (** Values, by their offset *)
}

let add_offset (self : state) i s = self.offset <- Int_map.add i s self.offset

let rec dump_rec (self : state) (off : offset) : unit =
  (* let off = Decode.deref_rec self.dec off in *)
  let decode_sub off =
    dump_rec self off;
    Decode.read ~auto_deref:false self.dec off
  in

  if not (Int_map.mem off self.offset) then (
    let v = Decode.read self.dec off in
    let printed =
      match v with
      | Null | True | False | Int _ | Float _ | String _ | Blob _ ->
        dump_primitive self.dec v
      | Array c ->
        let out = Buffer.create 32 in
        let l = Decode.Array_cursor.to_list_of decode_sub c in
        (match l with
        | [] -> bpf out "[]"
        | _ ->
          bpf out "Array(%d) [" (List.length l);
          List.iteri
            (fun i x ->
              if i > 0 then bpf out ",";
              bpf out "%s" (dump_primitive self.dec x))
            l;
          bpf out "]");
        Buffer.contents out
      | Dict c ->
        let l = Decode.Dict_cursor.to_list c in
        (match l with
        | [] -> "{}"
        | _ ->
          let out = Buffer.create 32 in
          bpf out "Map(%d) {" (List.length l);
          List.iteri
            (fun i (k, v) ->
              if i > 0 then bpf out ",";
              bpf out "%s: %s"
                (dump_primitive self.dec k)
                (dump_primitive self.dec v))
            l;
          bpf out "}";
          Buffer.contents out)
      | Pointer p ->
        let pointee = Decode.read self.dec @@ deref_rec self.dec p in
        spf "@0x%x (%s)" p (dump_primitive self.dec pointee)
      | Tag (c, x) ->
        let v = decode_sub x in
        spf "%d(%s)" c (dump_primitive self.dec v)
      | Cstor0 idx -> spf "C_%d" idx
      | Cstor1 (idx, p) ->
        let v = decode_sub p in
        spf "C_%d(%s)" idx (dump_primitive self.dec v)
      | CstorN (idx, c) ->
        let out = Buffer.create 32 in
        bpf out "C_%d(" idx;
        Decode.Array_cursor.to_list_of decode_sub c
        |> List.iteri (fun i v ->
               if i > 0 then bpf out ",";
               bpf out "%s" (dump_primitive self.dec v));
        bpf out ")";
        Buffer.contents out
    in
    add_offset self off printed
  )

let dump_slice (sl : slice) : string =
  let dec = Decode.create sl in
  let st = { offset = Int_map.empty; dec } in
  dump_rec st (Decode.get_entrypoint dec);
  let buf = Buffer.create 32 in
  Int_map.iter (fun off printed -> bpf buf "[0x%x]: %s\n" off printed) st.offset;
  Buffer.contents buf

let dump_string s : string = dump_slice (Slice.of_string s)
