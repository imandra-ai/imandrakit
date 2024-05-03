(** A data format designed to represent complex OCaml values.

    The format is designed for 0-copy parsing
    (or, not much copy anyway) and reasonable compactness.

    https://github.com/imandra-ai/imandrakit/discussions/2
  *)

include Types
module Encode = Encode
module Decode = Decode
module Dump = Dump
