(** Timestamp, in seconds *)

type t = (float[@printer Util.pp_duration_s]) [@@deriving show, eq]
(** Timestamps *)

let to_string_rfc3339 ?tz_offset_s (self : t) : string =
  let tz_offset_s =
    match tz_offset_s with
    | Some _ as r -> r
    | None -> Ptime_clock.current_tz_offset_s ()
  in
  Ptime.to_rfc3339 ?tz_offset_s
    (Ptime.of_float_s self |> Option.value ~default:(Ptime_clock.now ()))
