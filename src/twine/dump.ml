open struct
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

(* TODO:
   let rec deref (deser : CP.Deser.state) (i : int) =
     let c = CP.Private_.deser_heap_get deser i in
     match c with
     | `Tag (6, `Int j) -> deref deser j
     | _ -> c

   (** Dump immediate values, and a trivial summary for the rest *)
   let rec dump_immediate (c : CP.cbor) : string =
     match c with
     | `Null -> "null"
     | `Undefined -> "undefined"
     | `Simple i -> spf "s(%d)" i
     | `Int i -> spf "%d" i
     | `Bool b -> spf "%b" b
     | `Float f -> spf "%f" f
     | `Text s ->
       if String.length s > 20 then
         spf "%S[…%d omitted]" (String.sub s 0 20) (String.length s - 20)
       else
         spf "%S" s
     | `Bytes b -> spf "bytes(…[%d omitted])" (String.length b)
     | `Array l -> spf "[…[%d omitted]]" (List.length l)
     | `Map l -> spf "{…[%d omitted]}" (List.length l)
     | `Tag (6, `Int i) -> spf "@%d" i
     | `Tag (c, x) -> spf "%d(%s)" c (dump_immediate x)

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

   (** Dump a summary of the value, including small map/arrays *)
   let rec dump_summary (deser : CP.Deser.state) depth (c : CP.cbor) : string =
     let[@inline] recurse c =
       if depth <= 0 then
         dump_immediate c
       else
         dump_summary deser (depth - 1) c
     in
     match c with
     | `Null -> "null"
     | `Undefined -> "undefined"
     | `Simple i -> spf "s(%d)" i
     | `Int i -> spf "%d" i
     | `Bool b -> spf "%b" b
     | `Float f -> spf "%f" f
     | `Text s ->
       if String.length s > 20 then
         spf "%S[…%d omitted]" (String.sub s 0 20) (String.length s - 20)
       else
         spf "%S" s
     | `Bytes b -> dump_bytes_summary b
     | `Array l ->
       (match l with
       | x :: y :: z :: (_ :: _ as tl) ->
         spf "[%s,%s,%s,…(%d)]" (recurse x) (recurse y) (recurse z)
           (List.length tl)
       | _ -> spf "[%s]" (String.concat "," @@ List.map recurse l))
     | `Map l ->
       let ppkv (k, v) = spf "%s: %s" (recurse k) (recurse v) in
       (match l with
       | kv1 :: kv2 :: kv3 :: (_ :: _ as tl) ->
         spf "{%s,%s,%s,…(%d)]" (ppkv kv1) (ppkv kv2) (ppkv kv3) (List.length tl)
       | _ -> spf "{%s}" (String.concat "," @@ List.map ppkv l))
     | `Tag (6, `Int i) -> recurse (deref deser i)
     | `Tag (c, x) -> spf "%d(%s)" c (recurse x)

   let rec dump_c (deser : CP.Deser.state) (indent : int) (out : Buffer.t)
       (c : CP.cbor) : unit =
     match c with
     | `Null -> bpf out "null"
     | `Undefined -> bpf out "undefined"
     | `Simple i -> bpf out "s(%d)" i
     | `Int i -> bpf out "%d" i
     | `Bool b -> bpf out "%b" b
     | `Float f -> bpf out "%f" f
     | `Text s -> bpf out "%S" s
     | `Bytes b ->
       if string_is_printable b then
         bpf out "bytes(%S)" b
       else
         bpf out "bytes(h'%s')" (hex_of_string b)
     | `Array [] -> bpf out "[]"
     | `Array l ->
       bpf out "Array(%d) [" (List.length l);
       List.iteri
         (fun i x ->
           if i > 0 then bpf out ",";
           bpf out "\n%a %a" add_indent indent (dump_c deser (indent + 2)) x)
         l;
       bpf out " ]"
     | `Map [] -> bpf out "{}"
     | `Map l ->
       bpf out "Map(%d) {" (List.length l);
       List.iter
         (fun (x, y) ->
           bpf out "\n%a%a:\n%a%a" add_indent indent
             (dump_c deser (indent + 2))
             x add_indent (indent + 2)
             (dump_c deser (indent + 2))
             y)
         l;
       bpf out " }"
     | `Tag (6, `Int i) ->
       let pointee = deref deser i in
       bpf out "&%s @%d" (dump_summary deser 3 pointee) i
     | `Tag (c, x) -> bpf out "%d(%a)" c (dump_c deser indent) x

   let dump (out : Buffer.t) (self : CP.Deser.state) : unit =
     bpf out "heap:\n";
     CP.Private_.deser_heap_iter self (fun i x ->
         bpf out "  %06d: %a\n" i (dump_c self 4) x);
     bpf out "key: %a\n" (dump_c self 2) (CP.Private_.deser_key self);
     ()

   let dump_oc (oc : out_channel) self : unit =
     (* use local buffer to dump individual entries
        before writing them into [oc]. We don't buffer the whole
        output here as the value might be very large and we have the
        opportunity to wite to [oc] as we go. *)
     let buf = Buffer.create 128 in

     fpf oc "heap:\n";
     CP.Private_.deser_heap_iter self (fun i x ->
         Buffer.clear buf;
         bpf buf "  %06d: %a\n" i (dump_c self 4) x;
         Buffer.output_buffer oc buf);

     Buffer.clear buf;
     bpf buf "key: %a\n" (dump_c self 2) (CP.Private_.deser_key self);
     Buffer.output_buffer oc buf
*)

(* TODO:?

   let rec pp_diagnostic out (self : t) = ()
   (* TODO:
      match self with
      | Null -> Fmt.string out "null"
      (* | Undefined -> Fmt.string out "undefined" *)
      | Bool b -> Fmt.bool out b
      | Int i -> Fmt.int64 out i
      | Float f -> Fmt.float out f
      | Bytes b -> Fmt.fprintf out "h'%s'" (CCString.to_hex b)
      | Str s -> Fmt.fprintf out "%S" s
      | List l ->
        Fmt.fprintf out "[@[";
        List.iteri
          (fun i x ->
            if i > 0 then Fmt.fprintf out ",@ ";
            pp_diagnostic out x)
          l;
        Fmt.fprintf out "@]]"
      | Dict l ->
        Fmt.fprintf out "{@[";
        let i = ref 0 in
        Str_map.iter
          (fun k v ->
            if !i > 0 then Fmt.fprintf out ",@ ";
            incr i;
            Fmt.fprintf out "@[%S:@ %a@]" k pp_diagnostic v)
          l;
        Fmt.fprintf out "@]}"
      | Tag (i, x) -> Fmt.fprintf out "%d(@[%a@])" i pp_diagnostic x
   *)

   let show_diagnostic = Fmt.to_string pp_diagnostic
*)
