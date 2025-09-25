(** Log level *)

type json = Yojson.Safe.t

type t = Logs.level =
  | App
  | Error
  | Warning
  | Info
  | Debug
[@@deriving show { with_path = false }, enum, eq, ord, twine]

(** Print in a way that can be parsed back *)
let to_string : t -> string = function
  | App -> "app"
  | Debug -> "debug"
  | Error -> "error"
  | Info -> "info"
  | Warning -> "warn"

let to_yojson : t -> json = fun lvl -> `String (to_string lvl)

let of_yojson : json -> (t, string) result = function
  | `String "app" -> Ok App
  | `String "debug" -> Ok Debug
  | `String "error" -> Ok Error
  | `String "info" -> Ok Info
  | `String "warn" -> Ok Warning
  | `String lvl -> Error (spf "unknown log level %S" lvl)
  | _ -> Error "expected log level, got invalid json"

let parse str : _ option =
  match str with
  | "app" -> Some App
  | "debug" -> Some Debug
  | "error" -> Some Error
  | "info" -> Some Info
  | "warn" -> Some Warning
  | _ -> None
