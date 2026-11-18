(* Property tests for Imandrakit_leb128, on random values and random bytes.

   - Round trips: encode a value with [Encode] (C stubs), decode the bytes
     with [Decode] (OCaml), and check that the decoder returns the value and
     consumes exactly the bytes the encoder wrote. [skip] must consume as many
     bytes as [u64].
   - Windows: the decoder only depends on the bytes inside the slice's window
     [off, off + len), so the bytes around the window make no difference. *)

module Slice = Imandrakit_bytes.Byte_slice
module Buf = Imandrakit_bytes.Byte_buf
module D = Imandrakit_leb128.Decode
module E = Imandrakit_leb128.Encode
module G = QCheck2.Gen

let count : int = 10_000

let slice ?(off : int option) ?(len : int option) (s : string) : Slice.t =
  Slice.create ?off ?len (Bytes.of_string s)

let encode (enc : Buf.t -> int64 -> unit) (x : int64) : string =
  let buf = Buf.create () in
  enc buf x;
  Buf.contents buf

(** {1 Generators} *)

(* int64 values of every encoded length. A uniform int64 is almost always 10
   bytes long, so shift it right by 0 to 63 bits, then maybe negate it. *)
let any_length_int64 : int64 G.t =
  let open G in
  let+ x = int64 and+ shift = int_bound 63 and+ neg = bool in
  let x = Int64.shift_right_logical x shift in
  if neg then
    Int64.neg x
  else
    x

(* Arbitrary, often malformed, bytes; a window [off, off + len) inside them;
   and an offset into the window that may fall just outside it. *)
let bytes_with_window : (string * int * int * int) G.t =
  let open G in
  let* s = string_size (int_bound 16) in
  let n = String.length s in
  let* off = int_bound n in
  let* len = int_bound (n - off) in
  let+ pos = int_range (-1) (len + 1) in
  s, off, len, pos

(* {1 Properties} *)

let u64_round_trip : QCheck2.Test.t =
  QCheck2.Test.make ~count ~name:"u64 round trip" ~print:Int64.to_string
    any_length_int64 (fun x ->
      let s = encode E.u64 x in
      D.u64 (slice s) 0 = (x, String.length s))

let i64_round_trip : QCheck2.Test.t =
  QCheck2.Test.make ~count ~name:"i64 round trip" ~print:Int64.to_string
    any_length_int64 (fun x ->
      let s = encode E.i64 x in
      D.i64 (slice s) 0 = (x, String.length s))

let skip_agrees_with_u64 : QCheck2.Test.t =
  QCheck2.Test.make ~count ~name:"skip consumes as many bytes as u64"
    ~print:Int64.to_string any_length_int64 (fun x ->
      let sl = slice (encode E.u64 x) in
      D.skip sl 0 = snd (D.u64 sl 0))

(* ff is a continuation byte: a decoder that strays out of the window keeps
   reading instead of stopping. *)
let round_trip_in_padded_window : QCheck2.Test.t =
  QCheck2.Test.make ~count ~name:"round trip inside a window padded with ff"
    ~print:QCheck2.Print.(triple int64 int int)
    G.(triple any_length_int64 (int_bound 4) (int_bound 4))
    (fun (x, before, after) ->
      let s = encode E.u64 x in
      let pad n = String.make n '\xff' in
      let sl =
        slice ~off:before ~len:(String.length s) (pad before ^ s ^ pad after)
      in
      D.u64 sl 0 = (x, String.length s) && D.skip sl 0 = String.length s)

(* The bytes around the window make no difference, and malformed input raises
   Invalid_argument, never another exception. *)
let window_is_independent_of_buffer : QCheck2.Test.t =
  QCheck2.Test.make ~count ~name:"decoding a window = decoding a copy of it"
    ~print:QCheck2.Print.(quad string int int int)
    bytes_with_window
    (fun (s, off, len, pos) ->
      let result decode sl =
        match decode sl pos with
        | v -> Ok v
        | exception Invalid_argument msg -> Error msg
      in
      let in_buffer = slice ~off ~len s in
      let copy = slice (String.sub s off len) in
      result D.u64 in_buffer = result D.u64 copy
      && result D.skip in_buffer = result D.skip copy)

let () =
  QCheck_base_runner.run_tests_main
    [
      u64_round_trip;
      i64_round_trip;
      skip_agrees_with_u64;
      round_trip_in_padded_window;
      window_is_independent_of_buffer;
    ]
