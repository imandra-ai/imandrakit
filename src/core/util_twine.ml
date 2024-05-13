(** Utils for twine *)

module Twine = Imandrakit_twine

module Lazy = struct
  type 'a t = 'a lazy_t

  let to_twine p1 : _ lazy_t Twine.Encode.encoder =
   fun enc x -> p1 enc (Lazy.force x)

  let of_twine p1 : _ lazy_t Twine.Decode.decoder =
   fun dec x -> Lazy.from_val (p1 dec x)
end

module Str_map = struct
  let to_twine (ser_x : _ Twine.Encode.encoder) enc (m : _ Str_map.t) =
    Twine.Encode.(
      dict_iter enc
        (Str_map.to_iter m
        |> Iter.map (fun (k, v) -> Immediate.string k, ser_x enc v)))

  let of_twine (deser_x : _ Twine.Decode.decoder) dec c : _ Str_map.t =
    Twine.Decode.(
      let c = dict dec c in
      Dict_cursor.to_iter_of c (fun k v ->
          let k = string dec k in
          let v = deser_x dec v in
          k, v)
      |> Str_map.of_iter)
end

module Vec = struct
  let vec_to_twine ser_x : _ Vec.vector Twine.Encode.encoder =
   fun enc x ->
    Twine.Encode.(
      array_init enc (Vec.length x) (fun i -> ser_x enc @@ Vec.get x i))

  let vec_of_twine deser_x : _ Vec.vector Twine.Decode.decoder =
   fun dec c ->
    Twine.Decode.(
      let c = array dec c in
      let l = Array_cursor.to_list_of (deser_x dec) c in
      Vec.of_list l)
end

module Result = Imandrakit_twine.Util.Result
