(** A data format designed to represent complex OCaml values.

    The format is designed for 0-copy parsing
    (or, not much copy anyway) and reasonable compactness.

    https://github.com/imandra-ai/imandrakit/discussions/2


# Overview

The encoding relies on a first byte to disambiguate between values.
The first byte is a bitfield [kind:4 low:4] where:
  - [kind] is the kind of value (int, float, etc.)
  - [low] is a small integer value whose meaning depends on the kind.
    It can be a small integer, or a length, or a special value,
    or a constructor index. In case the value needs an integer argument [n]
    (length, actual integer, element count, etc.),  this
    integer argument is encoded in [low], but if [n>=15] then
      [low = 15] and [n-15] is encoded as LEB128 immediately
      after the first byte.

kinds:
- 0: special.
  n=0: false
  n=1: true
  n=2: nil
  Other values of [n] are reserved.
- 1: positive int, value is [n].
- 2: negative int, value is [-n-1]
- 3: float. if [n=0] then it's a float32, 4 bytes follow (little-endian);
  if [n=1] then it's a float64, 8 bytes follow (little-endian).
  Other values of [n] are reserved.
- 4: string, length in bytes is [n]. [n] bytes follow.
- 5: binary blob, length in bytes is [n]. [n] bytes follow.
- 6: array, number of elements is [n]. Immediate elements follow.
- 7: dict, number of key/value pairs is [n]. [2*n] immediate elements follow.
- 8: tag, the tag number is [n]. A single immediate element follows.
- 9: reserved
- 10: cstor0, constructor index is [n]
- 11: cstor1, constructor index is [n], a single immediate argument follows
- 12: cstorN, constructor index is [n], length as LEB128 follows
      (probably just one byte), then immediate arguments follow
- 13: reserved
- 14: ref, relative offset to the pointed value is [n].
  If we're at offset [i], then the reference points to [i-n-1].
  Not automatically dereferenced on parsing.
- 15: pointer, relative offset to the pointed value is [n].
  If we're at offset [i], then the pointer points to [i-n-1].

The toplevel value is written last, and to find it, a valid Twine blob
must end with a byte [n:u8] that indicates where this last value starts.
If the last byte [n] is at address [off], then the actual last value
starts at [off-n-1]. If the last value is too big, it's turned
into a pointer and that's what that last byte targets.

An immediate element is, basically, a non-recursive case that is easy to
{b skip} over to get to the next element. It includes booleans, integers,
floats, strings, pointers, but not arrays or dictionaries (which
must be encoded somewhere else and referred to by a pointer).

*)

include Types
module Immediate = Immediate
module Encode = Encode
module Decode = Decode
module Dump = Dump
module Util = Util

type immediate = Immediate.t
type offset = Types.offset [@@deriving eq, show, ord]

let[@inline] offset_to_twine _enc (o : offset) = Immediate.ref_ o
let offset_of_twine : offset Decode.decoder = Decode.ref_

type 'a encoder = 'a Encode.encoder
type 'a decoder = 'a Decode.decoder
