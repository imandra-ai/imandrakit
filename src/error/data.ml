(** Extensible fields for errors *)

module Twine = Imandrakit_twine

(** Information embedded in keys *)
module Key_info = struct
  type 'a t = {
    field_name: string;
    enc: 'a Twine.encoder;
    dec: 'a Twine.decoder;
    pp: 'a Fmt.printer;
  }
end

module M = Hmap.Make (Key_info)

module Key = struct
  type 'a t = 'a M.key
  type any = Any : _ t -> any

  let all_ : any Str_map.t Atomic.t = Atomic.make Str_map.empty
  let[@inline] pp (self : _ t) = (M.Key.info self).pp

  let make ~field_name ~enc ~dec ~pp () : _ t =
    let key_info = { Key_info.field_name; enc; dec; pp } in
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
let of_iter i = Iter.fold (fun m (M.B (k, v)) -> add k v m) empty i

let to_twine : t Twine.encoder =
 fun enc self ->
  Twine.Encode.(
    dict_iter enc (fun yield ->
        M.iter
          (fun (M.B (key, value)) ->
            let { Key_info.field_name; enc = enc_v; dec = _; pp = _ } =
              M.Key.info key
            in
            yield (Immediate.string field_name, enc_v enc value))
          self))

let of_twine : t Twine.decoder =
  Twine.Decode.(
    fun dec c ->
      let d = dict dec c in
      Dict_cursor.to_iter_of d (fun k v ->
          let k = string dec k in
          match Key.find k with
          | None -> failf "cannot find error field named %S" k
          | Some (Key.Any k) ->
            let key_info = M.Key.info k in
            let v = key_info.dec dec v in
            M.B (k, v))
      |> of_iter)

type binding = M.binding = B : 'a Key.t * 'a -> binding

let[@inline] iter self : binding Iter.t = fun k -> M.iter k self
