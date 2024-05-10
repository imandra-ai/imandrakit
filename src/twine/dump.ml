open Types

open struct
  module Slice = Byte_slice

  let bpf = Printf.bprintf
end

let hex_of_string = CCString.to_hex

let add_indent (out : Buffer.t) (indent : int) =
  for _i = 1 to indent do
    Buffer.add_char out ' '
  done

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

let rec dump_value_nonrec (self : Decode.t) (v : Decode.Value.t) =
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
  | Pointer p -> spf "@%x" p
  | Tag (n, v) -> spf "%d(@%x)" n v
  | Array c -> spf "[…[%d omitted]]" (Decode.Array_cursor.length c)
  | Dict c -> spf "{…[%d omitted]}" (Decode.Dict_cursor.length c)
  | Cstor0 idx -> spf "C_%d" idx
  | Cstor1 (idx, p) ->
    let a = Decode.read self @@ deref_rec self p in
    spf "C_%d(%s)" idx (dump_value_nonrec self a)
  | CstorN (idx, c) ->
    spf "C_%d(%s)" idx
      (String.concat ","
      @@ List.map (dump_value_nonrec self)
      @@ Decode.Array_cursor.to_list c)

(* Dump a summary of the value, including small map/arrays *)
let rec dump_summary (self : Decode.t) depth (out : Buffer.t)
    (v : Decode.Value.t) : unit =
  let[@inline] recurse out v =
    if depth <= 0 then
      Buffer.add_string out (dump_value_nonrec self v)
    else
      dump_summary self (depth - 1) out v
  in
  match v with
  | Null | True | False | Int _ | Float _ | String _ | Blob _ ->
    Buffer.add_string out @@ dump_value_nonrec self v
  | Array c ->
    let l = Decode.Array_cursor.to_list c in
    (match l with
    | x :: y :: z :: (_ :: _ as tl) ->
      bpf out "[%a,%a,%a,…(%d)]" recurse x recurse y recurse z (List.length tl)
    | _ ->
      bpf out "[";
      List.iteri
        (fun i v ->
          if i > 0 then bpf out ",";
          recurse out v)
        l;
      bpf out "]")
  | Dict c ->
    let l = Decode.Dict_cursor.to_list c in
    let ppkv out (k, v) = bpf out "%a: %a" recurse k recurse v in
    (match l with
    | kv1 :: kv2 :: kv3 :: (_ :: _ as tl) ->
      bpf out "{%a,%a,%a,…(%d)}" ppkv kv1 ppkv kv2 ppkv kv3 (List.length tl)
    | _ ->
      bpf out "{";
      List.iteri
        (fun i kv ->
          if i > 0 then bpf out ",";
          ppkv out kv)
        l;
      bpf out "}")
  | Tag (c, p) ->
    bpf out "%d(%a)" c recurse (Decode.read self @@ deref_rec self p)
  | Pointer p ->
    let v = Decode.read self @@ deref_rec self p in
    bpf out "&%a @%x" (dump_summary self 3) v p
  | Cstor0 idx -> bpf out "C_%d" idx
  | Cstor1 (idx, p) ->
    let a = Decode.read self @@ deref_rec self p in
    bpf out "C_%d(%a)" idx (dump_summary self (depth - 1)) a
  | CstorN (idx, c) ->
    bpf out "C_%d(" idx;
    Decode.Array_cursor.to_list c
    |> List.iteri (fun i v ->
           if i > 0 then bpf out ",";
           dump_summary self (depth - 1) out v);
    bpf out ")"

let rec dump_c (self : Decode.t) (indent : int) (out : Buffer.t)
    (v : Decode.Value.t) : unit =
  match v with
  | Null | True | False | Int _ | Float _ | String _ | Blob _ ->
    Buffer.add_string out @@ dump_value_nonrec self v
  | Array c ->
    let l = Decode.Array_cursor.to_list c in
    (match l with
    | [] -> bpf out "[]"
    | _ ->
      bpf out "Array(%d) [" (List.length l);
      List.iteri
        (fun i x ->
          if i > 0 then bpf out ",";
          bpf out "\n%a %a" add_indent indent (dump_c self (indent + 2)) x)
        l;
      bpf out " ]")
  | Dict c ->
    let l = Decode.Dict_cursor.to_list c in
    (match l with
    | [] -> bpf out "{}"
    | _ ->
      bpf out "Map(%d) {" (List.length l);
      List.iter
        (fun (x, y) ->
          bpf out "\n%a%a:\n%a%a" add_indent indent
            (dump_c self (indent + 2))
            x add_indent (indent + 2)
            (dump_c self (indent + 2))
            y)
        l;
      bpf out " }")
  | Pointer p ->
    let pointee = Decode.read self @@ deref_rec self p in
    bpf out "&%a @%d" (dump_summary self 3) pointee p
  | Tag (c, x) ->
    bpf out "%d(%a)" c (dump_c self indent)
      (Decode.read self @@ deref_rec self x)
  | Cstor0 idx -> bpf out "C_%d" idx
  | Cstor1 (idx, p) ->
    let a = Decode.read self @@ deref_rec self p in
    bpf out "C_%d(%a)" idx (dump_summary self 3) a
  | CstorN (idx, c) ->
    bpf out "C_%d(" idx;
    Decode.Array_cursor.to_list c
    |> List.iteri (fun i v ->
           if i > 0 then bpf out ",";
           dump_summary self 3 out v);
    bpf out ")"

let dump_slice (sl : slice) : string =
  let buf = Buffer.create 32 in
  let dec = Decode.create sl in
  let v = Decode.read_entrypoint dec in
  dump_c dec 0 buf v;
  Buffer.contents buf

let dump_string s : string = dump_slice (Slice.of_string s)
