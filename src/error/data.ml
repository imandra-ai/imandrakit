(** Extensible fields for errors *)

module Ser_pack = Imandrakit_ser_pack

(** Information embedded in keys *)
module Key_info = struct
  type 'a t = {
    field_name: string;
    enc: 'a Ser_pack.Ser.t;
    dec: 'a Ser_pack.Deser.t;
  }
end

module M = Hmap.Make (Key_info)

module Key = struct
  type 'a t = 'a M.key
  type any = Any : _ t -> any

  let all_ : any Str_map.t Atomic.t = Atomic.make Str_map.empty

  let make ~field_name ~enc ~dec () : _ t =
    let key_info = { Key_info.field_name; enc; dec } in
    let key = M.Key.create key_info in
    (* add to the map *)
    while
      let old = Atomic.get all_ in
      assert (not (Str_map.mem field_name old));
      not
        (Atomic.compare_and_set all_ old (Str_map.add field_name (Any key) old))
    do
      ()
    done;
    key

  let find k = Str_map.get k (Atomic.get all_)
end

type 'a key = 'a Key.t
type t = M.t

let empty = M.empty
let is_empty = M.is_empty
let get = M.find
let add = M.add
let fold = M.fold

let to_serpack : t Ser_pack.Ser.t =
 fun st self ->
  let m =
    fold
      (fun (B (key, value)) (m : _ Str_map.t) ->
        let { Key_info.field_name; enc; dec = _ } = M.Key.info key in
        Str_map.add field_name (enc st value) m)
      self Str_map.empty
  in
  Ser_pack.Ser.dict m

let of_serpack : t Ser_pack.Deser.t =
  Ser_pack.Deser.(
    fun st c ->
      let d = to_map st c in
      List.fold_left
        (fun m (k, v) ->
          match Key.find k with
          | None -> failf "cannot find error field named %S" k
          | Some (Key.Any k) ->
            let key_info = M.Key.info k in
            let v = key_info.dec st v in
            add k v m)
        empty d)
