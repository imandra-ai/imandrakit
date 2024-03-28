module Ser = Imandrakit_ser
module V = Ser.Value

type t = Ser.Value.t

exception Error of string
exception Indefinite

let[@inline] fail msg = raise (Error msg)

let[@inline] i64_to_int i =
  let j = Int64.to_int i in
  if Int64.(of_int j = i) then
    j
  else
    fail "int64 does not fit in int"

let decode_exn (s : string) : t =
  let b = Bytes.unsafe_of_string s in
  let i = ref 0 in

  (* currently at end delimiter? *)
  let[@inline] is_break_stop_code () = Char.code s.[!i] = 0b111_11111 in

  let[@inline] read_i8 () =
    let c = Char.code s.[!i] in
    incr i;
    c
  in

  let[@inline] read_i16 () =
    let c = Bytes.get_uint16_be b !i in
    i := !i + 2;
    c
  in

  let[@inline] read_i32 () =
    let c = Bytes.get_int32_be b !i in
    i := !i + 4;
    c
  in

  let[@inline] read_i64 () =
    let c = Bytes.get_int64_be b !i in
    i := !i + 8;
    c
  in

  let reserve_n n =
    let j = !i in
    if j + n > String.length s then fail "cbor: cannot extract slice";
    i := !i + n;
    j
  in

  (* read integer value from least significant bits *)
  let read_int ~allow_indefinite low : int64 =
    match low with
    | _ when low < 0 -> fail "cbor: invalid length"
    | _ when low < 24 -> Int64.of_int low
    | 24 -> Int64.of_int (read_i8 ())
    | 25 -> Int64.of_int (read_i16 ())
    | 26 -> Int64.of_int32 (read_i32 ())
    | 27 -> read_i64 ()
    | 28 | 29 | 30 -> fail "cbor: invalid length"
    | 31 ->
      if allow_indefinite then
        raise_notrace Indefinite
      else
        fail "cbor: invalid integer 31 in this context"
    | _ -> assert false
  in

  (* appendix D

     double decode_half(unsigned char *halfp) {
       unsigned half = (halfp[0] << 8) + halfp[1];
       unsigned exp = (half >> 10) & 0x1f;
       unsigned mant = half & 0x3ff;
       double val;
       if (exp == 0) val = ldexp(mant, -24);
       else if (exp != 31) val = ldexp(mant + 1024, exp - 25);
       else val = mant == 0 ? INFINITY : NAN;
       return half & 0x8000 ? -val : val;
     }
  *)
  let decode_f16 (half : int) : float =
    (* exponent is bits 15:10 *)
    let exp = (half lsr 10) land 0x1f in
    (* mantissa is bits 9:0 *)
    let mant = half land 0x3ff in
    let value =
      if exp = 0 then
        ldexp (float mant) (-24)
      else if exp <> 31 then
        ldexp (float (mant + 1024)) (exp - 25)
      else if mant = 0 then
        infinity
      else
        nan
    in
    if half land 0x8000 <> 0 then
      -.value
    else
      value
  in

  (* roughly follow https://www.rfc-editor.org/rfc/rfc8949.html#pseudocode *)
  let rec read_value () : V.t =
    let c = read_i8 () in
    let high = (c land 0b111_00000) lsr 5 in
    let low = c land 0b000_11111 in
    match high with
    | 0 -> V.int64 (read_int ~allow_indefinite:false low)
    | 1 ->
      let i = read_int ~allow_indefinite:false low in
      V.int64 @@ Int64.(sub minus_one i)
    | 2 ->
      let s = read_bytes ~ty:`Bytes low in
      V.bytes s
    | 3 ->
      let s = read_bytes ~ty:`String low in
      V.string s
    | 4 ->
      let l =
        match read_int ~allow_indefinite:true low |> i64_to_int with
        | len -> List.init len (fun _ -> read_value ())
        | exception Indefinite ->
          let l = ref [] in
          while not (is_break_stop_code ()) do
            l := read_value () :: !l
          done;
          incr i;
          (* consume stop code *)
          List.rev !l
      in
      V.list l
    | 5 ->
      let dict =
        match read_int ~allow_indefinite:true low |> i64_to_int with
        | len -> List.init len (fun _ -> read_pair ()) |> Str_map.of_list
        | exception Indefinite ->
          let dict = ref Str_map.empty in
          while not (is_break_stop_code ()) do
            let k, v = read_pair () in
            dict := Str_map.add k v !dict
          done;
          incr i;
          (* consume stop code *)
          !dict
      in
      V.dict dict
    | 6 ->
      let tag = read_int ~allow_indefinite:false low |> i64_to_int in
      let v = read_value () in
      V.tag tag v
    | 7 ->
      (* simple or float,
         https://www.rfc-editor.org/rfc/rfc8949.html#fpnocont *)
      let i = read_int ~allow_indefinite:false low in
      (match low with
      | 20 -> V.bool false
      | 21 -> V.bool true
      | 22 -> V.null
      | 23 -> V.null (* undefined -> null *)
      | _ when low <= 24 ->
        (* simple -> int *)
        V.int64 i
      | 25 ->
        (* float16 *)
        V.float (decode_f16 (Int64.to_int i))
      | 26 ->
        (* float 32 *)
        V.float (Int32.float_of_bits (Int64.to_int32 i))
      | 27 ->
        (* float 64 *)
        V.float (Int64.float_of_bits i)
      | 28 | 29 | 30 -> fail "cbor: malformed"
      | 31 -> fail "uncaught 'break' stop code"
      | _ -> assert false (* unreachable *))
    | _ ->
      (* unreachable *)
      assert false
  and read_bytes ~ty low =
    match read_int ~allow_indefinite:true low |> i64_to_int with
    | exception Indefinite ->
      let buf = Buffer.create 32 in
      while not (is_break_stop_code ()) do
        match read_value (), ty with
        | Str s, `String | Bytes s, `Bytes -> Buffer.add_string buf s
        | _ -> fail "cbor: invalid chunk in indefinite length string/byte"
      done;
      incr i;
      (* consume stop code *)
      Buffer.contents buf
    | len ->
      let off = reserve_n len in
      String.sub s off len
  and read_pair () =
    let k = read_value () in
    let v = read_value () in
    let k =
      match k with
      | V.Str k -> k
      | _ -> fail "dictionary must have string keys"
    in
    k, v
  in
  read_value ()

let decode s = try Ok (decode_exn s) with Failure s -> Error s

let encode ?(buf = Buffer.create 32) (self : t) : string =
  Buffer.clear buf;

  let[@inline] add_byte (high : int) (low : int) =
    let i = (high lsl 5) lor low in
    assert (i land 0xff == i);
    Buffer.add_char buf (Char.unsafe_chr i)
  in

  let add_i64 (i : int64) = Buffer.add_int64_be buf i in

  (* add unsigned integer, including first tag byte *)
  let add_uint (high : int) (x : int64) =
    assert (x >= 0L);
    if x < 24L then
      add_byte high (i64_to_int x)
    else if x <= 0xffL then (
      add_byte high 24;
      Buffer.add_char buf (Char.unsafe_chr (i64_to_int x))
    ) else if x <= 0xff_ffL then (
      add_byte high 25;
      Buffer.add_uint16_be buf (i64_to_int x)
    ) else if x <= 0xff_ff_ff_ffL then (
      add_byte high 26;
      Buffer.add_int32_be buf (Int64.to_int32 x)
    ) else (
      add_byte high 27;
      Buffer.add_int64_be buf x
    )
  in

  let encode_str s =
    add_uint 3 (Int64.of_int (String.length s));
    Buffer.add_string buf s
  in

  let rec encode_val (self : t) : unit =
    match self with
    | Bool false -> add_byte 7 20
    | Bool true -> add_byte 7 21
    | Null -> add_byte 7 22
    | Float f ->
      add_byte 7 27;
      (* float 64 *)
      add_i64 (Int64.bits_of_float f)
    | List l ->
      add_uint 4 (Int64.of_int (List.length l));
      List.iter encode_val l
    | Dict l ->
      add_uint 5 (Int64.of_int (Str_map.cardinal l));
      Str_map.iter
        (fun k v ->
          encode_str k;
          encode_val v)
        l
    | Str s -> encode_str s
    | Bytes s ->
      add_uint 2 (Int64.of_int (String.length s));
      Buffer.add_string buf s
    | Tag (t, v) ->
      add_uint 6 (Int64.of_int t);
      encode_val v
    | Int i ->
      if i >= Int64.zero then
        add_uint 0 i
      else if Int64.(add min_int 2L) > i then (
        (* large negative int, be careful. encode [(-i)-1] via int64. *)
        add_byte 1 27;
        Buffer.add_int64_be buf Int64.(neg (add 1L i))
      ) else
        add_uint 1 Int64.(sub (neg i) one)
  in
  encode_val self;
  Buffer.contents buf

let rec pp_diagnostic out (self : t) =
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

let show_diagnostic = Fmt.to_string pp_diagnostic
