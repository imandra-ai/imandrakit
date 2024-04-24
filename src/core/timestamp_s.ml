(** Wall-clock timestamps as a float *)

type t = (float[@printer Util.pp_duration_s]) [@@deriving show, eq, serpack]
(** Timestamps *)

let now : unit -> t = Util.ptime_now

let to_string_rfc3339 ?tz_offset_s (self : t) : string =
  let tz_offset_s =
    match tz_offset_s with
    | Some _ as r -> r
    | None -> Ptime_clock.current_tz_offset_s ()
  in
  Ptime.to_rfc3339 ?tz_offset_s
    (Ptime.of_float_s self |> Option.value ~default:(Ptime_clock.now ()))

let of_string_rfc3339 (s : string) : (t, [ `Msg of string ]) result =
  match Ptime.of_rfc3339 s |> Ptime.rfc3339_error_to_msg with
  | Ok (t, _, _) -> Ok (Ptime.to_float_s t)
  | Error _ as err -> err
