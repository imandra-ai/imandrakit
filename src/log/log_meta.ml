module Err = Imandrakit_error.Error

(** Meta data *)
type t =
  | Null
  | String of string
  | Bool of bool
  | Int of int
  | Float of float
  | List of t list
  | Assoc of (string * t) list
[@@deriving show { with_path = false }, twine, eq]

let k_meta : t Hmap.key = Hmap.Key.create ()

type json = Yojson.Safe.t

let rec to_yojson (m : t) : json =
  match m with
  | Null -> `Null
  | String s -> `String s
  | Bool b -> `Bool b
  | Int i -> `Int i
  | Float f -> `Float f
  | List l -> `List (List.map to_yojson l)
  | Assoc l -> `Assoc (List.map (fun (k, v) -> k, to_yojson v) l)

let to_trace_data (m : t) : Trace.user_data =
  match m with
  | Null -> `None
  | String s -> `String s
  | Bool b -> `Bool b
  | Int i -> `Int i
  | Float f -> `Float f
  | List _ | Assoc _ -> `String (show m)

open struct
  let json_error = Err.Kind.make ~name:"LogJsonError" ()
end

let rec of_yojson (j : json) : t =
  match j with
  | `Null -> Null
  | `String s -> String s
  | `Bool b -> Bool b
  | `Float f -> Float f
  | `Intlit i ->
    (try Int (int_of_string i)
     with _ -> Err.failf ~kind:json_error "invalid int lit %S" i)
  | `Int i -> Int i
  | `List l -> List (List.map of_yojson l)
  | `Assoc l -> Assoc (List.map (fun (k, v) -> k, of_yojson v) l)
  | `Tuple _ | `Variant _ ->
    Err.failf ~kind:json_error "unsupported json for Log_event.meta"
