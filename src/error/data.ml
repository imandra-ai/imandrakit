(** Extensible fields for errors *)

module Ser = Imandrakit_ser

(** Information embedded in keys *)
module Key_info = struct
  type 'a t = {
    field_name: string;
    codec: 'a Ser.Codec.t;
  }
end

module M = Hmap.Make (Key_info)

module Key = struct
  type 'a t = 'a M.key
  type any = Any : _ t -> any

  let all_ : any Str_map.t Atomic.t = Atomic.make Str_map.empty

  let make ~field_name ~codec () : _ t =
    let key_info = { Key_info.field_name; codec } in
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

let encode (self : t) : Ser.Value.t =
  let m =
    fold
      (fun (B (key, value)) (m : _ Str_map.t) ->
        let { Key_info.field_name; codec } = M.Key.info key in
        Str_map.add field_name (Ser.Codec.encode codec value) m)
      self Str_map.empty
  in
  Ser.Value.dict m

let decode : t Ser.Decode.t =
  Ser.Decode.(
    let* d = dict_as_list in
    fold_l
      (fun m (k, v) ->
        match Key.find k with
        | None -> failf "cannot find error field named %S" k
        | Some (Key.Any k) ->
          let key_info = M.Key.info k in
          let+ v = apply key_info.codec.dec v in
          add k v m)
      empty d)

let codec : t Ser.Codec.t =
  Ser.Codec.create ~name:"error_fields" ~enc:encode ~dec:decode ()
