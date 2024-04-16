(** Core utils for Imandra.

    This provides common functionalities and foundations for
    our projects. *)

include Imandrakit_common

(** {2 Re-exports} *)

module Error = Imandrakit_error.Error
module Error_core = Imandrakit_error.Error_core
module Error_kind = Imandrakit_error.Kind
module Error_data = Imandrakit_error.Data
module Ser_value = Imandrakit_ser.Value
module Ser_pack = Imandrakit_ser_pack

(** {2 Local modules} *)

module Ansi_clean = Ansi_clean
module Apool = Apool
module Backtrack_stack = Backtrack_stack
module Backtrackable_tbl = Backtrackable_tbl
module Bag = Bag
module Basic_bv = Basic_bv
module Bidir_tbl = Bidir_tbl
module Bit_field = Bit_field
module Codec = Codec
module Core_classes = Core_classes
module Dot = Dot
module Duration_s = Duration_s
module F_queue = F_queue
module Gen_id = Gen_id
module Hashcons = Hashcons
module Log_ctx = Log_ctx
module Log_event = Log_event
module Log_level = Log_level
module Log_reader = Log_reader
module Logger = Logger
module Observer = Observer
module Scc = Scc
module Stats = Stats
module Timestamp_s = Timestamp_s
module Trace = Trace_core
module Trace_async = Trace_async
module Util = Util
module Util_b64 = Util_b64
module Util_gzip = Util_gzip
module Util_serpack = Util_serpack
module Util_sockaddr = Util_sockaddr
module Util_zip = Util_zip
module Util_zlib = Util_zlib
