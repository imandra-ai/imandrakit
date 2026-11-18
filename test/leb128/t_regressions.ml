(* Regression tests for the LEB128 decoder

   Related issues: #28

   A slice is a window [off, off + len) into a larger buffer, and decoder
   offsets count from the start of the window. *)

module Slice = Imandrakit_bytes.Byte_slice
module Buf = Imandrakit_bytes.Byte_buf
module D = Imandrakit_leb128.Decode
module E = Imandrakit_leb128.Encode

let slice ?(off : int option) ?(len : int option) (s : string) : Slice.t =
  Slice.create ?off ?len (Bytes.of_string s)

let encode (enc : Buf.t -> int64 -> unit) (x : int64) : string =
  let buf = Buf.create () in
  enc buf x;
  Buf.contents buf

let raises_invalid_arg (msg : string) (f : unit -> _) : bool =
  match f () with
  | _ -> false
  | exception Invalid_argument m -> m = msg

let out_of_bound (f : unit -> _) : bool = raises_invalid_arg "out of bound" f

let too_long (f : unit -> _) : bool =
  raises_invalid_arg "leb128 varint is too long" f

(** {1 Reads stay inside the window}

    The old decoder only checked that the window was non-empty, then read with
    [Bytes.get], which is bounded by the whole buffer, not the window. *)

(* A varint running past the end of the window: 80 asks for another byte, but
   the 01 is outside the window.
   Before: u64 read the 01 and returned 128 (2 bytes); skip returned 2. *)
let () =
  let sl = slice ~len:1 "\x80\x01" in
  assert (out_of_bound (fun () -> D.u64 sl 0));
  assert (out_of_bound (fun () -> D.skip sl 0))

(* An offset before the window, which is the 06.
   Before: u64 read the 05 and returned 5 (1 byte); skip, which also ignored
   where the window starts (see the next section), raised Bytes.get's "index
   out of bounds". *)
let () =
  let sl = slice ~off:1 ~len:1 "\x05\x06\x07" in
  assert (out_of_bound (fun () -> D.u64 sl (-1)));
  assert (out_of_bound (fun () -> D.skip sl (-1)))

(* An offset at the end of the window, which is the 06.
   Before: u64 read the 07 and returned 7 (1 byte); skip returned 1. *)
let () =
  let sl = slice ~off:1 ~len:1 "\x05\x06\x07" in
  assert (out_of_bound (fun () -> D.u64 sl 1));
  assert (out_of_bound (fun () -> D.skip sl 1))

(* An empty window.
   Before: raised too, the one case the old check caught. *)
let () =
  let sl = slice ~len:0 "\x05" in
  assert (out_of_bound (fun () -> D.u64 sl 0));
  assert (out_of_bound (fun () -> D.skip sl 0))

(** {1 skip starts at the window}

    [u64] started at [sl.off + off], but [skip] started at [off]. *)

(* A window starting at buffer byte 1: the window is the 05, and the ff
   before it is a continuation byte.
   Before: skip started at the ff and returned 2. *)
let () =
  let sl = slice ~off:1 ~len:1 "\xff\x05" in
  assert (D.u64 sl 0 = (5L, 1));
  assert (D.skip sl 0 = 1)

(** {1 Over-long varints are rejected}

    A u64 fits in 10 bytes, and the 10th holds only bit 63. The old decoder
    checked this only on a varint's last byte, so a 10th byte with the
    continuation bit was accepted. *)

(* 10 bytes, the 10th carrying one bit: the longest valid u64, 2^64 - 1. *)
let () =
  let sl = slice (String.make 9 '\xff' ^ "\x01") in
  assert (D.u64 sl 0 = (-1L, 10));
  assert (D.skip sl 0 = 10)

(* 10 bytes, the 10th carrying two bits.
   Before: rejected too, by the last-byte check. *)
let () =
  let sl = slice (String.make 9 '\xff' ^ "\x02") in
  assert (too_long (fun () -> D.u64 sl 0));
  assert (too_long (fun () -> D.skip sl 0))

(* 11 bytes.
   Before: accepted. The 01 was shifted left by 70 bits, which is unspecified
   and acted as a shift by 6: u64 returned 64 (11 bytes); skip returned 11. *)
let () =
  let sl = slice (String.make 10 '\x80' ^ "\x01") in
  assert (too_long (fun () -> D.u64 sl 0));
  assert (too_long (fun () -> D.skip sl 0))

(** {1 Zigzag decoding uses a logical shift}

    Zigzag decoding is [(v lsr 1) lxor (-(v land 1))] on an unsigned [v]. An
    arithmetic shift copies the top bit of [v] down, and that bit is set exactly
    when |x| >= 2^62. *)

let () =
  let two_62 = Int64.shift_left 1L 62 in
  List.iter
    (fun x -> assert (fst (D.i64 (slice (encode E.i64 x)) 0) = x))
    [
      (* Before: decoded correctly, the largest such value *)
      Int64.sub two_62 1L;
      (* Before: -2^62 *)
      two_62;
      (* Before: -1 *)
      Int64.max_int;
      (* Before: decoded correctly *)
      Int64.neg two_62;
      (* Before: 2^62 - 1 *)
      Int64.(sub (neg two_62) 1L);
      (* Before: 0 *)
      Int64.min_int;
    ]

(** {1 Known encodings}

    Round trips check that [Encode] and [Decode] agree with each other, so they
    would miss both drifting from LEB128 together. These pin the exact bytes, at
    the ends of the 7-bit groups and of int64. [u64] treats int64 as unsigned,
    so min_int is 2^63 and -1 is 2^64 - 1. *)

let () =
  List.iter
    (fun (x, bytes) ->
      assert (encode E.u64 x = bytes);
      assert (D.u64 (slice bytes) 0 = (x, String.length bytes)))
    [
      0L, "\x00";
      1L, "\x01";
      127L, "\x7f";
      128L, "\x80\x01";
      16383L, "\xff\x7f";
      16384L, "\x80\x80\x01";
      Int64.max_int, String.make 8 '\xff' ^ "\x7f";
      Int64.min_int, String.make 9 '\x80' ^ "\x01";
      -1L, String.make 9 '\xff' ^ "\x01";
    ]
