(** Utils for ser-pack *)

module Ser_pack = Imandrakit_ser_pack

module Lazy = struct
  type 'a t = 'a lazy_t

  let to_serpack p1 : _ lazy_t Ser_pack.Ser.t =
   fun ser x -> p1 ser (Lazy.force x)

  let of_serpack p1 : _ lazy_t Ser_pack.Deser.t =
   fun deser x -> Lazy.from_val (p1 deser x)
end

module Str_map = struct
  let to_serpack (ser_x : _ Ser_pack.Ser.t) st (m : _ Str_map.t) =
    Ser_pack.Ser.(
      map
        (Str_map.to_iter m
        |> Iter.map (fun (k, v) -> k, ser_x st v)
        |> Iter.to_rev_list))

  let of_serpack (deser_x : _ Ser_pack.Deser.t) st c : _ Str_map.t =
    Ser_pack.Deser.(
      let l = to_map st c in
      List.fold_left
        (fun m (k, v) ->
          let v = deser_x st v in
          Str_map.add k v m)
        Str_map.empty l)
end

module Vec = struct
  let vec_to_serpack ser_x : _ Vec.vector Ser_pack.Ser.t =
   fun st x -> Ser_pack.Ser.(list_of ser_x st @@ Vec.to_list x)

  let vec_of_serpack deser_x : _ Vec.vector Ser_pack.Deser.t =
   fun st c ->
    Ser_pack.Deser.(
      let l = to_list_of deser_x st c in
      Vec.of_list l)
end

module Result = struct
  let iter_result fok ferr = function
    | Ok x -> fok x
    | Error e -> ferr e

  type ('a, 'err) t = ('a, 'err) result [@@deriving show, map, iter]

  let to_serpack p1 p2 st = function
    | Ok x -> Ser_pack.Ser.(list [ string "ok"; p1 st x ])
    | Error e -> Ser_pack.Ser.(list [ string "err"; p2 st e ])

  let of_serpack p1 p2 : _ Ser_pack.Deser.t =
    Ser_pack.Deser.(
      fun st c ->
        let l = to_list st c in
        match l with
        | [ Str "ok"; x ] ->
          let x = p1 st x in
          Ok x
        | [ Str "err"; e ] ->
          let e = p2 st e in
          Error e
        | _ -> fail "expected result")
end
