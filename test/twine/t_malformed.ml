(* Malformed inputs for the Twine decoder

   Related issues: #28

   The decoder must raise [Imandrakit_twine.Error] on malformed input: never
   another exception, a wrong value or a crash. The expected output records
   what it does now, with one of these results:
   - OK: decoded, followed by the value. Expected only for the controls and
     the "accepted:" cases: a constructor or record with more items than its
     type declares decodes, and the extra items are ignored, so that a
     program built with an older type can read data from a newer one that
     added items at the end (commit 2f9b86f2).
   - Error: raised [Imandrakit_twine.Error], followed by the message.
   - OTHER: raised another exception.
   - DIED: the process crashed.

   The cases are decoded from a string ([D.decode_string]). A few are also
   decoded from a file ([D.of_in_channel]): a channel reads bytes through its
   own functions, and is decoded without the guard of [D.decode_string]. The
   rest of the decoder is shared, so the other cases would not differ. Each
   decode runs in a forked child, so that a crash is reported instead of
   killing the test.

   "Before:" notes what the decoder did before the fixes for issue #28, from
   running these cases against it. Cases without a note behaved the same.

   Reading the bytes in the cases below:
   - A value's first byte is [high nibble = type, low nibble = small value].
     A low nibble of 15 means a LEB128 varint follows, and the value is the
     varint + 15.
   - Types: 0 special (0 false, 1 true, 2 null), 1 int, 3 float (low nibble 0
     f32, 1 f64), 4 string, 6 array, 9 reserved, 10 constructor without
     arguments, 11 constructor with one argument, 12 constructor with a LEB128
     argument count, 14 ref, 15 pointer.
   - Refs and pointers point back: the target is [offset - n - 1].
   - The last byte [c] says where the top-level value starts:
     [entry = last - c - 1], where [last] is the index of the last byte. *)

module D = Imandrakit_twine.Decode

(** {1 Types decoded with the ppx-generated decoders} *)

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

(* Leaf is constructor 0, Node constructor 1. The generated decoder recurses
   once per Node. *)
type nested =
  | Leaf
  | Node of nested
[@@deriving twine]

(** {1 Decoders}

    Each [decode_as_*] decodes a value with one decoder and shows it. *)

let decode_as_int64 : string D.decoder =
 fun dec off -> Int64.to_string (D.int64 dec off)

let decode_as_string : string D.decoder =
 fun dec off -> Printf.sprintf "%S" (D.string dec off)

let decode_as_float : string D.decoder =
 fun dec off -> string_of_float (D.float dec off)

(* The untyped value, without following a ref. *)
let decode_as_raw_value : string D.decoder =
 fun dec off -> D.Value.show (D.read dec off)

let decode_as_int_array : string D.decoder =
 fun dec off ->
  D.array dec off
  |> D.Array_cursor.to_array_of (D.int_truncate dec)
  |> Array.to_list |> List.map string_of_int |> String.concat "; "
  |> Printf.sprintf "[|%s|]"

(* The untyped elements, read one after the other. *)
let decode_as_value_array : string D.decoder =
 fun dec off ->
  D.array dec off |> D.Array_cursor.to_list |> List.map D.Value.show
  |> String.concat "; " |> Printf.sprintf "[|%s|]"

let decode_as_t : string D.decoder =
 fun dec off ->
  match of_twine dec off with
  | A (x, y) -> Printf.sprintf "A (%d, %d)" x y
  | B -> "B"
  | C x -> Printf.sprintf "C %d" x
  | D { x; y } -> Printf.sprintf "D { x = %d; y = %d }" x y

let decode_as_c : string D.decoder =
 fun dec off -> Printf.sprintf "%C" (c_of_twine dec off)

let decode_as_r : string D.decoder =
 fun dec off ->
  let { a; b } = r_of_twine dec off in
  Printf.sprintf "{ a = %d; b = %d }" a b

(* Shows the depth. *)
let decode_as_nested : string D.decoder =
 fun dec off ->
  let rec depth = function
    | Leaf -> 0
    | Node n -> 1 + depth n
  in
  Printf.sprintf "depth %d" (depth (nested_of_twine dec off))

(** {1 Cases} *)

type case = {
  name: string;
  decode: string D.decoder;
  bytes: string;
}

(* [Node (Node ... Leaf)] with [n] Nodes, as the encoder lays it out:
   b1 a0 (a Node with Leaf inline), then [n - 1] times b1 f2 (a Node with a
   pointer to the Node before it), then 01 (entry = the last Node). *)
let nested_nodes (n : int) : string =
  let buf = Buffer.create ((2 * n) + 1) in
  Buffer.add_string buf "\xb1\xa0";
  for _ = 2 to n do
    Buffer.add_string buf "\xb1\xf2"
  done;
  Buffer.add_char buf '\x01';
  Buffer.contents buf

(** {2 From a string} *)

let cases : case list =
  [
    {
      name = "control: int 5";
      decode = decode_as_int64;
      (* 15: int 5. 00: entry = 1 - 0 - 1 = 0 *)
      bytes = "\x15\x00";
    };
    {
      name = "control: 3 nested nodes";
      decode = decode_as_nested;
      bytes = nested_nodes 3;
    };
    {
      name = "wrong type: int read as string";
      decode = decode_as_string;
      bytes = "\x15\x00";
    };
    {
      name = "bad tag: reserved type 9";
      decode = decode_as_raw_value;
      (* 90: type 9 *)
      bytes = "\x90\x00";
    };
    {
      name = "bad tag: float with low nibble 7";
      decode = decode_as_float;
      (* 37: float with low nibble 7; only 0 (f32) and 1 (f64) exist.
         08: entry 0.
         Before: OK 0. *)
      bytes = "\x37" ^ String.make 8 '\x00' ^ "\x08";
    };
    {
      name = "bad tag: unknown constructor index";
      decode = decode_as_t;
      (* a5: constructor 5 without arguments; t's are 0 to 3 *)
      bytes = "\xa5\x00";
    };
    {
      name = "out of bounds: empty input";
      decode = decode_as_raw_value;
      (* no last byte to find the entry from.
         Before: OTHER Assert_failure. *)
      bytes = "";
    };
    {
      name = "out of bounds: entrypoint byte larger than input";
      decode = decode_as_raw_value;
      (* ff: entry = 1 - 255 - 1 = -255.
         Before: read outside the input and returned OK (Int 3L). *)
      bytes = "\x15\xff";
    };
    {
      name = "out of bounds: truncated float64";
      decode = decode_as_float;
      (* 31: f64, whose 8 bytes would start at offset 1; only 00 is there.
         Before: OTHER Invalid_argument("index out of bounds"). *)
      bytes = "\x31\x00";
    };
    {
      name = "out of bounds: string of length 5 with 3 bytes left";
      decode = decode_as_string;
      (* 45: string of length 5; only "ab" and the entry byte follow.
         02: entry 0.
         Before: OK "ab\002", cut short and including the entry byte. *)
      bytes = "\x45ab\x02";
    };
    {
      name = "out of bounds: ref before the start of the input";
      decode = decode_as_raw_value;
      (* 00: entry 1. e5: ref with n = 5, target 1 - 5 - 1 = -5. A ref is
         returned without being followed, so nothing reads at -5.
         Before: OK (Ref -5). *)
      bytes = "\x15\xe5\x00";
    };
    {
      name = "out of bounds: pointer at offset 1 with n=5";
      decode = decode_as_raw_value;
      (* 00: entry 1. f5: pointer with n = 5, target 1 - 5 - 1 = -5.
         Before: read outside the input and returned OK False. *)
      bytes = "\x15\xf5\x00";
    };
    {
      name = "out of bounds: pointer with n=2^40";
      decode = decode_as_raw_value;
      (* 06: entry 1. ff: pointer with a varint at 2 of 2^40, target about
         -1.1e12.
         Before: segfault. *)
      bytes = "\x15\xff" ^ String.make 5 '\x80' ^ "\x20\x06";
    };
    {
      name = "bad integer: LEB128 longer than 64 bits";
      decode = decode_as_int64;
      (* 1f: int with a varint at 1. Its 10th byte, 7f, carries 7 bits where
         only 1 fits in 64. 0a: entry 0.
         Before: OTHER Invalid_argument("leb128 varint is too long"). *)
      bytes = "\x1f" ^ String.make 9 '\xff' ^ "\x7f\x0a";
    };
    {
      name = "bad integer: LEB128 with 11 bytes";
      decode = decode_as_int64;
      (* 1f: int with a varint at 1 whose 10th byte, 80, asks for an 11th.
         0b: entry 0.
         Before: OK 79, from 64 (the 01 shifted by 70, acting as 6) + 15. *)
      bytes = "\x1f" ^ String.make 10 '\x80' ^ "\x01\x0b";
    };
    {
      name = "bad integer: int of 2^64 - 1 + 15";
      decode = decode_as_int64;
      (* 1f: int with a varint at 1 of 2^64 - 1, a valid varint too large for
         int64. 0a: entry 0.
         Before: OK 14. The varint wrapped to -1, then + 15. *)
      bytes = "\x1f" ^ String.make 9 '\xff' ^ "\x01\x0a";
    };
    {
      name = "bad integer: ref distance of 2^63 + 15";
      decode = decode_as_raw_value;
      (* 0a: entry 1. ef: ref with a varint at 2 of 2^63, so n = 2^63 + 15.
         Before: OK (Ref -15). Int64.to_int dropped the top bit, leaving
         n = 15 and a target of 1 - 15 - 1 = -15. *)
      bytes = "\x15\xef" ^ String.make 9 '\x80' ^ "\x01\x0a";
    };
    {
      name = "bad integer: constructor index 2^62";
      decode = decode_as_raw_value;
      (* af: constructor without arguments, index from a varint at 1 of
         2^62 - 15, so 2^62: fits in int64 but not in OCaml's 63-bit int.
         09: entry 0.
         Before: OK (Cstor0 -4611686018427387904), wrapped around. *)
      bytes = "\xaf\xf1" ^ String.make 7 '\xff' ^ "\x3f\x09";
    };
    {
      name = "bad integer: array of 2^60 elements";
      decode = decode_as_int_array;
      (* 6f: array with a count from a varint at 1 of 2^60 - 15, so 2^60
         elements in an 11-byte input. 15: element 0, int 5 (it is decoded
         before the array is allocated). 0a: entry 0.
         Before: OTHER Invalid_argument("Array.make"). *)
      bytes = "\x6f\xf1" ^ String.make 7 '\xff' ^ "\x0f\x15\x0a";
    };
    {
      name = "bad integer: char field given 256";
      decode = decode_as_c;
      (* 1f: int with a varint at 1 of f1 01 = 241, so 241 + 15 = 256, one
         more than a char can hold. 02: entry 0.
         Before: OTHER Invalid_argument("Char.chr"). *)
      bytes = "\x1f\xf1\x01\x02";
    };
    {
      name = "too deep: 1M nested nodes";
      decode = decode_as_nested;
      (* Each Node is decoded by a recursive call. Not fixed: Stack_overflow
         is not turned into Error. *)
      bytes = nested_nodes 1_000_000;
    };
    {
      name = "accepted: 3 args for a constructor of arity 2";
      decode = decode_as_t;
      (* c0: constructor 0 (A). 03: 3 arguments, 11 12 13: ints 1, 2, 3.
         04: entry 0 *)
      bytes = "\xc0\x03\x11\x12\x13\x04";
    };
    {
      name = "accepted: 2 args for a constructor of arity 1";
      decode = decode_as_t;
      (* c2: constructor 2 (C) with a count. 02: 2 arguments, 11 12: ints 1,
         2. 03: entry 0 *)
      bytes = "\xc2\x02\x11\x12\x03";
    };
    {
      name = "accepted: 3 fields for an inline record of 2";
      decode = decode_as_t;
      (* c3: constructor 3 (D, an inline record). 03: 3 arguments, 11 12 13:
         ints 1, 2, 3. 04: entry 0 *)
      bytes = "\xc3\x03\x11\x12\x13\x04";
    };
    {
      name = "accepted: 3 fields for a record of 2";
      decode = decode_as_r;
      (* 63: array of 3. 11 12 13: ints 1, 2, 3. 03: entry 0 *)
      bytes = "\x63\x11\x12\x13\x03";
    };
  ]

(** {2 From a channel}

    One case per read function of the channel, plus one for the missing guard.
*)

let channel_cases : case list =
  [
    { name = "control: int 5"; decode = decode_as_int64; bytes = "\x15\x00" };
    {
      name = "out of bounds: empty input";
      decode = decode_as_raw_value;
      (* [length].
         Before: OTHER Assert_failure. *)
      bytes = "";
    };
    {
      name = "out of bounds: array element past the end";
      decode = decode_as_value_array;
      (* [read_char] at 5, past the end. 62: array of 2, which the count
         check allows in 5 bytes. 43: element 0, a string of length 3: 61 61
         and the entry byte 03 (entry = 4 - 3 - 1 = 0). Element 1 would start
         at 5.
         Before: OTHER Invalid_argument("option is None"). *)
      bytes = "\x62\x43\x61\x61\x03";
    };
    {
      name = "out of bounds: truncated float32";
      decode = decode_as_float;
      (* [read_int32] of 4 bytes at offset 1, where only 1 is left.
         Before: OK 0. *)
      bytes = "\x30\x00";
    };
    {
      name = "out of bounds: truncated float64";
      decode = decode_as_float;
      (* [read_int64] of 8 bytes at offset 1, where only 1 is left.
         Before: OK with a value read from uninitialised bytes, which varied
         between runs (2.18755989899e-314, 2.15786334422e-314). *)
      bytes = "\x31\x00";
    };
    {
      name = "out of bounds: string of length 5 with 3 bytes left";
      decode = decode_as_string;
      (* [read_blob] of 5 bytes at offset 1, where only 3 are left.
         Before: OK "ab\002", cut short and including the entry byte. *)
      bytes = "\x45ab\x02";
    };
    {
      name = "out of bounds: varint running past the end";
      decode = decode_as_int_array;
      (* [read_leb128], which reads at most 16 bytes but fewer near the end.
         For the varint to reach the last byte, that byte must be >= 0x80, so
         the entry is at least 129 bytes before it and an array of 1-byte
         ints bridges the gap. 00: unused. 6f 6d: array of 109 + 15 = 124
         elements. 10 x 123: int 0. 1f: int with a varint at 127: 80 80 80,
         then the last byte 80 (entry = 130 - 128 - 1 = 1), all with the
         continuation bit.
         Before: OTHER Invalid_argument("index out of bounds"). *)
      bytes =
        "\x00\x6f\x6d" ^ String.make 123 '\x10' ^ "\x1f" ^ String.make 4 '\x80';
    };
    {
      name = "bad integer: char field given 256";
      decode = decode_as_c;
      (* Not fixed: [Char.chr] raises Invalid_argument, which
         [D.decode_string] turns into Error, but nothing does from a
         channel. *)
      bytes = "\x1f\xf1\x01\x02";
    };
  ]

(** {1 Running the cases} *)

type source =
  | From_string
  | From_channel

let run_decoder (source : source) (decoder : 'a D.decoder) (bytes : string) : 'a
    =
  match source with
  | From_string -> D.decode_string decoder bytes
  | From_channel ->
    let tmp_file = Filename.temp_file "imandrakit." ".twine" in
    Fun.protect ~finally:(fun () -> Sys.remove tmp_file) @@ fun () ->
    Out_channel.with_open_bin tmp_file (fun oc -> output_string oc bytes);
    In_channel.with_open_bin tmp_file @@ fun ic ->
    let dec = D.of_in_channel ic in
    decoder dec (D.deref_rec dec (D.get_entrypoint dec))

(* Print the case name, then decode in a forked child, which prints the
   result. *)
let run_case (source : source) { name; decode; bytes } : unit =
  (* flush before forking, or the child would print the name again *)
  Printf.printf "%s\n  %!" name;
  (match Unix.fork () with
  | 0 ->
    (* 1M words (8 MB), so that a 2 MB input of nested nodes exhausts it *)
    Gc.set { (Gc.get ()) with stack_limit = 1_000_000 };
    (match run_decoder source decode bytes with
    | v -> Printf.printf "OK: %s\n" v
    | exception Imandrakit_twine.Error m -> Printf.printf "Error: %s\n" m
    | exception e -> Printf.printf "OTHER: %s\n" (Printexc.to_string e));
    exit 0
  | pid ->
    (match Unix.waitpid [] pid with
    | _, WEXITED 0 -> ()
    | _, WEXITED n -> Printf.printf "DIED: exit %d\n" n
    | _, (WSIGNALED _ | WSTOPPED _) -> Printf.printf "DIED: signal\n"));
  print_newline ()

let () =
  print_endline "From a string\n==============";
  List.iter (run_case From_string) cases;
  print_endline "From a channel\n==============";
  List.iter (run_case From_channel) channel_cases
