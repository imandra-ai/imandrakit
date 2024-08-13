(** Events.

    The log reporter emits events, which can then be written to
    various outputs. *)

module Err = Imandrakit_error.Error

type t = {
  lvl: Log_level.t;  (** Log level. *)
  ts: float;  (** Timestamp, in seconds, since the UNIX epoch. *)
  msg: string;  (** Log message. *)
  src: string;  (** Log source. *)
  meta: (string * string) list;  (** Additional metadata *)
}
[@@deriving show { with_path = false }, serpack, twine]
(** A log event, which we can store, serialize, send elsewhere, etc. *)

type json = Yojson.Safe.t

(** Convert log events to JSON *)
let to_yojson : t -> json =
 fun ev ->
  let { lvl; ts; msg; src; meta } = ev in
  let msg = Ansi_clean.remove_escape_codes msg in
  (* pack metadata in a dictionary *)
  let meta_dict : (string * json) list =
    match meta with
    | [] -> []
    | _ -> [ "meta", `Assoc (List.map (fun (k, v) -> k, `String v) meta) ]
  in
  `Assoc
    (meta_dict
    @ [
        "lvl", Log_level.to_yojson lvl;
        "ts", `Float ts;
        "msg", `String msg;
        "src", `String src;
      ])

let json_error = Err.Kind.make ~name:"LogJsonError" ()

let of_yojson_ (j : json) : t Err.result =
  let module JU = Yojson.Safe.Util in
  let unwrap_ ctx = function
    | Ok x -> x
    | Error s -> Err.failf ~kind:json_error "%s: %s" ctx s
  in
  try
    let lvl =
      JU.member "lvl" j |> Log_level.of_yojson |> unwrap_ "parsing level"
    in
    let ts = JU.member "ts" j |> JU.to_float in
    let msg = JU.member "msg" j |> JU.to_string in
    let src = JU.member "src" j |> JU.to_string in
    let meta =
      match JU.member "meta" j with
      | exception _ -> []
      | `Null -> []
      | `Assoc l -> List.map (fun (k, v) -> k, JU.to_string v) l
      | _ -> Err.fail ~kind:json_error "expected null or object for 'meta'"
    in
    Ok { lvl; ts; msg; src; meta }
  with
  | Failure e ->
    Error (Err.mk_errorf ~kind:json_error "invalid log event: %s" e)
  | _e ->
    let bt = Printexc.get_raw_backtrace () in
    let err =
      Err.of_exn ~bt ~kind:json_error _e
      |> Err.add_ctx (Err.message "decoding log event from JSON")
    in
    Error err

let of_yojson (j : json) : (t, string) result =
  of_yojson_ j |> Result.map_error Err.show
