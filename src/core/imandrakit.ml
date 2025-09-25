(** Core utils for Imandra.

    This provides common functionalities and foundations for our projects. *)

include Imandrakit_common
include Imandrakit_bytes

(** {2 Re-exports} *)

module Error = Imandrakit_error.Error
module Error_core = Imandrakit_error.Error_core
module Error_kind = Imandrakit_error.Kind
module Error_data = Imandrakit_error.Data
module Ser_value = Imandrakit_ser.Value

(** {2 Local modules} *)

module Ansi_clean = Ansi_clean
module Apool = Apool
module Backtrack_stack = Backtrack_stack
module Backtrackable_tbl = Backtrackable_tbl
module Bag = Bag
module Basic_bv = Basic_bv
module Bidir_tbl = Bidir_tbl
module Bit_field = Bit_field
module Byte_buf = Byte_buf
module Byte_slice = Byte_slice
module Codec = Codec
module Core_classes = Core_classes
module Dot = Dot
module Duration_s = Duration_s
module F_queue = F_queue
module Gen_id = Gen_id
module Hashcons = Hashcons
module Observer = Observer
module Scc = Scc
module Stats = Stats
module Timestamp = Timestamp
module Timestamp_s = Timestamp_s
module Trace = Trace_core
module Util = Util
module Util_b64 = Util_b64
module Util_sockaddr = Util_sockaddr
module Util_twine = Util_twine
