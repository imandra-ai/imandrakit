(* Malformed inputs (issue #28). The decoder must raise [Imandrakit_twine.Error]
   on each of them: never another exception, a wrong value or a crash.

   Each case runs in a forked child so that a crash is reported as a line of
   output instead of killing the test. The expected output records what the
   decoder does now, so a case that still prints OK or OTHER is not fixed yet.

   Reading the bytes in the cases below:
   - A value's first byte is [high nibble = type, low nibble = small value].
     A low nibble of 15 means a LEB128 varint follows, and the value is the
     varint + 15.
   - Types: 0 special (0 false, 1 true, 2 null), 1 int, 3 float (low nibble 0
     f32, 1 f64), 4 string, 9 reserved, 10 constructor without arguments, 12
     constructor with a LEB128 argument count, 14 ref, 15 pointer.
   - Refs and pointers point back: the target is [offset - n - 1].
   - The last byte [c] says where the top-level value starts:
     [entry = last - c - 1], where [last] is the index of the last byte. *)

module D = Imandrakit_twine.Decode

(* Constructor indices: A is 0, B is 1, C is 2, D is 3. *)
type t =
  | A of int * int
  | B
  | C of int
  | D of {
      x: int;
      y: int;
    }
[@@deriving twine]

(* Encoded as an array of its fields. *)
type r = {
  a: int;
  b: int;
}
[@@deriving twine]

(* Encoded as an int; the generated decoder converts it with [Char.chr]. *)
type c = char [@@deriving twine]

let repeat_byte n c = String.make n c

(* Decode from a string, or from a file through [D.of_in_channel]. *)
type source_kind =
  | From_string
  | From_channel

let run_decoder src_kind (decoder : 'a D.decoder) (bytes : string) : 'a =
  match src_kind with
  | From_string -> D.decode_string decoder bytes
  | From_channel ->
    let tmp_file = Filename.temp_file "imandrakit." ".twine" in
    Fun.protect ~finally:(fun () -> Sys.remove tmp_file) @@ fun () ->
    Out_channel.with_open_bin tmp_file (fun oc -> output_string oc bytes);
    In_channel.with_open_bin tmp_file @@ fun ic ->
    let dec = D.of_in_channel ic in
    decoder dec (D.deref_rec dec (D.get_entrypoint dec))

(* Each [decode_as_*] decodes [bytes] with one decoder and shows the result. *)

include struct
  let decode_as_int64 src_kind bytes =
    Int64.to_string (run_decoder src_kind D.int64 bytes)

  let decode_as_string src_kind bytes =
    Printf.sprintf "%S" (run_decoder src_kind D.string bytes)

  let decode_as_float src_kind bytes =
    string_of_float (run_decoder src_kind D.float bytes)

  (* the untyped value, without following a ref *)
  let decode_as_raw_value src_kind bytes =
    run_decoder src_kind (fun dec off -> D.Value.show (D.read dec off)) bytes

  let decode_as_int_array src_kind bytes =
    run_decoder src_kind
      (fun dec off ->
        D.array dec off
        |> D.Array_cursor.to_array_of (D.int_truncate dec)
        |> Array.to_list |> List.map string_of_int |> String.concat "; "
        |> Printf.sprintf "[|%s|]")
      bytes

  (* with the ppx-generated decoder for [t] *)
  let decode_as_t src_kind bytes =
    match run_decoder src_kind of_twine bytes with
    | A (x, y) -> Printf.sprintf "A (%d, %d)" x y
    | B -> "B"
    | C x -> Printf.sprintf "C %d" x
    | D { x; y } -> Printf.sprintf "D { x = %d; y = %d }" x y

  (* with the ppx-generated decoder for [c] *)
  let decode_as_c src_kind bytes =
    Printf.sprintf "%C" (run_decoder src_kind c_of_twine bytes)

  (* with the ppx-generated decoder for [r] *)
  let decode_as_r src_kind bytes =
    let { a; b } = run_decoder src_kind r_of_twine bytes in
    Printf.sprintf "{ a = %d; b = %d }" a b
end

(** Two valid controls, then inputs the decoder must reject: malformed bytes, or
    valid bytes read with the wrong decoder. Last, extra trailing items, which
    the decoder accepts by design.

    Each case is:
    - [name]
    - [src_kind]: whether decoded from a string or from a file.
    - [decode_and_show]
    - [bytes]: the input *)
let cases :
    (string * source_kind * (source_kind -> string -> string) * string) list =
  [
    (* 15: int 5. 00: entry = 1 - 0 - 1 = 0 *)
    "control: int 5", From_string, decode_as_int64, "\x15\x00";
    "control: int 5 (channel)", From_channel, decode_as_int64, "\x15\x00";
    (* a valid int, read with the string decoder *)
    "int read as string", From_string, decode_as_string, "\x15\x00";
    (* 90: type 9, which is reserved *)
    "reserved tag 9", From_string, decode_as_raw_value, "\x90\x00";
    (* a5: constructor 5 without arguments; t has only 0 and 1 *)
    "unknown constructor index", From_string, decode_as_t, "\xa5\x00";
    (* ff: entry = 1 - 255 - 1 = -255 *)
    ( "entrypoint byte larger than input",
      From_string,
      decode_as_raw_value,
      "\x15\xff" );
    (* no last byte to find the entry from *)
    "empty input", From_string, decode_as_raw_value, "";
    (* 31: f64, whose 8 bytes would start at offset 1; only 00 is there *)
    "truncated float64", From_string, decode_as_float, "\x31\x00";
    "truncated float64 (channel)", From_channel, decode_as_float, "\x31\x00";
    (* 1f: int with a varint at 1. Its 10th byte, 7f, carries 7 bits where
       only 1 fits in 64. 0a: entry 0 *)
    ( "LEB128 longer than 64 bits",
      From_string,
      decode_as_int64,
      "\x1f" ^ repeat_byte 9 '\xff' ^ "\x7f\x0a" );
    (* 1f: int with a varint at 1 whose 10th byte, 80, asks for an 11th.
       0b: entry 0 *)
    ( "LEB128 with 11 bytes",
      From_string,
      decode_as_int64,
      "\x1f" ^ repeat_byte 10 '\x80' ^ "\x01\x0b" );
    (* 37: float with low nibble 7; only 0 and 1 are valid. 08: entry 0 *)
    ( "float with low nibble 7",
      From_string,
      decode_as_float,
      "\x37" ^ repeat_byte 8 '\x00' ^ "\x08" );
    (* 45: string of length 5; only "ab" and the entry byte follow.
       02: entry 0 *)
    ( "string of length 5 with 3 bytes left",
      From_string,
      decode_as_string,
      "\x45ab\x02" );
    ( "string of length 5 with 3 bytes left (channel)",
      From_channel,
      decode_as_string,
      "\x45ab\x02" );
    (* 1f: int with a varint at 1 of 2^64-1, a valid varint too large for
       int64: it wraps to -1, then + 15 = 14. 0a: entry 0 *)
    ( "LEB128 encoding of 2^64-1",
      From_string,
      decode_as_int64,
      "\x1f" ^ repeat_byte 9 '\xff' ^ "\x01\x0a" );
    (* 0a: entry 1. ef: ref with a varint at 2 of 2^63, so n = 2^63 + 15.
       Int64.to_int drops the top bit, leaving n = 15 and a target of
       1 - 15 - 1 = -15 *)
    ( "ref distance truncated by Int64.to_int",
      From_string,
      decode_as_raw_value,
      "\x15\xef" ^ repeat_byte 9 '\x80' ^ "\x01\x0a" );
    (* 00: entry 1. e5: ref with n = 5, target 1 - 5 - 1 = -5. A ref is
       returned without being followed, so nothing reads at -5 *)
    ( "ref pointing before the start of the input",
      From_string,
      decode_as_raw_value,
      "\x15\xe5\x00" );
    (* af: constructor without arguments, index from a varint at 1 of
       2^62 - 15, so 2^62: fits in int64 but not in OCaml's 63-bit int.
       09: entry 0 *)
    ( "constructor index 2^62, too large for int",
      From_string,
      decode_as_raw_value,
      "\xaf\xf1" ^ repeat_byte 7 '\xff' ^ "\x3f\x09" );
    (* 6f: array with a count from a varint at 1 of 2^60 - 15, so 2^60
       elements in an 11-byte input. 15: element 0, int 5 (it is decoded
       before the array is allocated). 0a: entry 0 *)
    ( "array claiming 2^60 elements",
      From_string,
      decode_as_int_array,
      "\x6f\xf1" ^ repeat_byte 7 '\xff' ^ "\x0f\x15\x0a" );
    (* 00: entry 1. f5: pointer with n = 5, target 1 - 5 - 1 = -5 *)
    ( "pointer at offset 1 with n=5",
      From_string,
      decode_as_raw_value,
      "\x15\xf5\x00" );
    (* 06: entry 1. ff: pointer with a varint at 2 of 2^40, target about
       -1.1e12; this used to segfault *)
    ( "pointer with n=2^40",
      From_string,
      decode_as_raw_value,
      "\x15\xff" ^ repeat_byte 5 '\x80' ^ "\x20\x06" );
    (* 1f: int with a varint at 1 of f1 01 = 241, so 241 + 15 = 256, one
       more than a char can hold. 02: entry 0 *)
    "char field given 256", From_string, decode_as_c, "\x1f\xf1\x01\x02";
    (* Extra trailing items, accepted by design: a constructor or record with
       more items than its type declares decodes, and the extra items are
       ignored. This lets a program built with an older type read data from a
       newer one that added items at the end (commit 2f9b86f2). These cases
       pin that behaviour. *)

    (* c0: constructor 0 (A). 03: 3 arguments, 11 12 13: ints 1, 2, 3; A takes
       2. 04: entry 0 *)
    ( "extra arg ignored: 3 args, arity 2",
      From_string,
      decode_as_t,
      "\xc0\x03\x11\x12\x13\x04" );
    (* c2: constructor 2 (C) with a count. 02: 2 arguments, 11 12: ints 1, 2;
       C takes 1. 03: entry 0 *)
    ( "extra arg ignored: 2 args, arity 1",
      From_string,
      decode_as_t,
      "\xc2\x02\x11\x12\x03" );
    (* c3: constructor 3 (D, an inline record). 03: 3 arguments, 11 12 13:
       ints 1, 2, 3; D has 2 fields. 04: entry 0 *)
    ( "extra arg ignored: inline record, 3 for 2",
      From_string,
      decode_as_t,
      "\xc3\x03\x11\x12\x13\x04" );
    (* 63: array of 3. 11 12 13: ints 1, 2, 3; r has 2 fields. 03: entry 0 *)
    ( "extra field ignored: record, 3 for 2",
      From_string,
      decode_as_r,
      "\x63\x11\x12\x13\x03" );
  ]

let run_case (name, src_kind, decode_and_show, bytes) =
  (* flush before forking, or the child would print the name again *)
  Printf.printf "%-48s %!" name;
  match Unix.fork () with
  | 0 ->
    (match decode_and_show src_kind bytes with
    | v -> Printf.printf "%-6s %s\n" "OK" v
    | exception Imandrakit_twine.Error m -> Printf.printf "%-6s %s\n" "Error" m
    | exception e -> Printf.printf "%-6s %s\n" "OTHER" (Printexc.to_string e));
    exit 0
  | pid ->
    (match Unix.waitpid [] pid with
    | _, WEXITED 0 -> ()
    | _, WEXITED n -> Printf.printf "%-6s exit %d\n%!" "DIED" n
    | _, (WSIGNALED _ | WSTOPPED _) -> Printf.printf "%-6s signal\n%!" "DIED")

let () =
  Printf.printf "%-48s %-6s %s\n" "case" "result" "value or message";
  List.iter run_case cases
