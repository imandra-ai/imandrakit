module TLS = Thread_local_storage
module Json = Yojson.Safe
module Log = (val Logs.src_log (Logs.Src.create "x.debug"))

let debug_error = Error_kind.make ~name:"debug_server" ()
