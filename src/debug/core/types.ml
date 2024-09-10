type span = int64 [@@deriving show, yojson, twine]

let _ : span -> Trace_core.span = Fun.id

type tid = int [@@deriving show, yojson, twine]
type pid = int [@@deriving show, yojson, twine]

type user_data = Trace_subscriber.user_data =
  | U_bool of bool
  | U_float of float
  | U_int of int
  | U_none
  | U_string of string
[@@deriving show { with_path = false }, yojson, twine]

type flavor = Trace_subscriber.flavor =
  | Sync
  | Async
[@@deriving show { with_path = false }, yojson, twine]

type level = Trace_core.Level.t =
  | Error
  | Warning
  | Info
  | Debug1
  | Debug2
  | Debug3
  | Trace
[@@deriving show { with_path = false }, yojson, twine]

type span_info = {
  __FUNCTION__: string option;
  __FILE__: string;
  __LINE__: int;
  name: string;
  time_ns: float;
  data: (string * user_data) list;
}
[@@deriving show { with_path = false }, yojson, twine]
