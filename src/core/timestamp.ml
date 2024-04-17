(** Wall-clock timestamps via Ptime *)

open struct
  module Ser_pack = Imandrakit_ser_pack
end

type t = Ptime.t [@@deriving eq]

let show (self : t) : string = Ptime.to_rfc3339 self
let pp = Fmt.of_to_string show
let now : unit -> t = Ptime_clock.now
let to_yojson self = `String (Ptime.to_rfc3339 ~space:false self)

let of_yojson = function
  | `String s ->
    Ptime.of_rfc3339 s |> Ptime.rfc3339_error_to_msg
    |> Result.map_error (fun (`Msg s) -> s)
  | _ -> Error "expected a RFC3339 timestamp"

let to_serpack : t Ser_pack.Ser.t =
  Ser_pack.Ser.(fun _st self -> string (Ptime.to_rfc3339 ~space:false self))

let of_serpack : t Ser_pack.Deser.t =
  Ser_pack.Deser.(
    fun st v ->
      let d = to_text st v in
      match Ptime.of_rfc3339 d |> Ptime.rfc3339_error_to_msg with
      | Ok (t, _, _) -> t
      | Error (`Msg msg) -> failf "invalid timestamp: %s" msg)
