(** Log level *)

type json = Yojson.Safe.t

type t = Logs.level =
  | App
  | Error
  | Warning
  | Info
  | Debug
[@@deriving show { with_path = false }, enum, eq, ord, serpack, twine]

let to_yojson : t -> json = function
  | App -> `String "app"
  | Debug -> `String "debug"
  | Error -> `String "error"
  | Info -> `String "info"
  | Warning -> `String "warn"

let of_yojson : json -> (t, string) result = function
  | `String "app" -> Ok App
  | `String "debug" -> Ok Debug
  | `String "error" -> Ok Error
  | `String "info" -> Ok Info
  | `String "warn" -> Ok Warning
  | `String lvl -> Error (spf "unknown log level %S" lvl)
  | _ -> Error "expected log level, got invalid json"
