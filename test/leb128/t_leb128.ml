(* Tests for Imandrakit_leb128. The output is compared with t_leb128.expected.

   What is tested:

   - Round trips. [Encode] (C stubs) and [Decode] (OCaml) agree on 5 edge
     values and 10,000 random values of every byte length, for [u64], for
     [i64] (zigzag), and inside a window that starts partway into a larger
     buffer. [skip] returns the same byte count as [u64].
   - Edge values. The exact bytes of values at the 7-bit group boundaries and
     at the ends of int64 (read as unsigned, so -1 is 2^64-1), and that they
     decode back.
   - Malformed input, for both [u64] and [skip]:
     - Window bounds: the decoder reads only bytes inside the slice's window
       ([off] to [off + len - 1]). A varint running past the end of the
       window, an offset outside it and an empty slice raise
       [Invalid_argument], and offsets count from the window's start.
     - Length: a u64 fits in 10 bytes, and the 10th byte carries one bit. A
       10th byte with more, or an 11th byte, raises [Invalid_argument]. *)

module Slice = Imandrakit_bytes.Byte_slice
module Buf = Imandrakit_bytes.Byte_buf
module D = Imandrakit_leb128.Decode
module E = Imandrakit_leb128.Encode

let encode (enc : Buf.t -> int64 -> unit) (x : int64) : string =
  let buf = Buf.create () in
  enc buf x;
  Buf.contents buf

let slice ?off ?len s = Slice.create ?off ?len (Bytes.of_string s)

let hex s =
  String.to_seq s |> List.of_seq
  |> List.map (fun c -> Printf.sprintf "%02x" (Char.code c))
  |> String.concat " "

(* Run [f] and describe its result or exception. *)
let show (f : unit -> string) : string =
  match f () with
  | s -> s
  | exception Invalid_argument msg -> Printf.sprintf "Invalid_argument %S" msg

let u64 sl off () =
  let v, n = D.u64 sl off in
  Printf.sprintf "%Ld (%d bytes)" v n

let skip sl off () = Printf.sprintf "%d bytes" (D.skip sl off)

(* Round trips
   ===========

   [Encode] goes through C stubs, [Decode] is OCaml: check that they agree on
   values of every byte length. Values come from a fixed seed. *)

let random_int64 st : int64 =
  let x = Random.State.bits64 st in
  let x = Int64.shift_right_logical x (Random.State.int st 64) in
  if Random.State.bool st then
    x
  else
    Int64.neg x

let round_trip name (check : int64 -> bool) =
  let st = Random.State.make [| 42 |] in
  let n = 10_000 in
  let edge = [ 0L; 1L; -1L; Int64.max_int; Int64.min_int ] in
  let values = edge @ List.init n (fun _ -> random_int64 st) in
  match List.find_opt (fun x -> not (check x)) values with
  | None -> Printf.printf "%-44s ok (%d values)\n" name (List.length values)
  | Some x -> Printf.printf "%-44s FAILED on %Ld\n" name x

let () =
  round_trip "u64 round trip" (fun x ->
      let s = encode E.u64 x in
      D.u64 (slice s) 0 = (x, String.length s));
  round_trip "i64 round trip" (fun x ->
      let s = encode E.i64 x in
      D.i64 (slice s) 0 = (x, String.length s));
  round_trip "skip agrees with u64" (fun x ->
      let sl = slice (encode E.u64 x) in
      D.skip sl 0 = snd (D.u64 sl 0));
  (* inside a window with junk on both sides *)
  round_trip "round trip inside a window" (fun x ->
      let s = encode E.u64 x in
      let sl =
        slice ~off:3 ~len:(String.length s) ("\xff\xff\xff" ^ s ^ "\xff\xff")
      in
      D.u64 sl 0 = (x, String.length s) && D.skip sl 0 = String.length s)

(* Edge values
   ===========

   The exact bytes for values at the ends of the 7-bit groups and of int64,
   read as unsigned. *)

let () =
  print_newline ();
  List.iter
    (fun x ->
      let s = encode E.u64 x in
      Printf.printf "%-22Ld %-32s %s\n" x (hex s) (show (u64 (slice s) 0)))
    [
      0L;
      1L;
      127L;
      128L;
      16383L;
      16384L;
      Int64.max_int;
      Int64.min_int (* unsigned 2^63 *);
      -1L (* unsigned 2^64-1 *);
    ]

(* Malformed input
   ===============

   Each case is decoded with [u64] and with [skip]. The decoder may only read
   bytes inside the slice's window, and a u64 fits in 10 bytes, the 10th
   carrying one bit. *)

let () =
  print_newline ();
  List.iter
    (fun (name, sl, off) ->
      Printf.printf "%s\n  u64:  %s\n  skip: %s\n" name
        (show (u64 sl off))
        (show (skip sl off)))
    [
      (* the window holds only 0x80, whose continuation bit asks for 0x01 *)
      "varint running past the end of the window", slice ~len:1 "\x80\x01", 0;
      "offset -1", slice ~off:1 ~len:1 "\x05\x06\x07", -1;
      "offset at the end of the window", slice ~off:1 ~len:1 "\x05\x06\x07", 1;
      "empty slice", slice ~len:0 "\x05", 0;
      (* buffer byte 0 is a continuation byte; the window starts after it *)
      "window starting at buffer offset 1", slice ~off:1 ~len:1 "\xff\x05", 0;
      "10th byte carrying two bits", slice (String.make 9 '\xff' ^ "\x02"), 0;
      "11 bytes", slice (String.make 10 '\x80' ^ "\x01"), 0;
    ]
