module LS = Moonpool.Task_local_storage

type 'a tag = 'a Logs.Tag.def

(** Storage key for the ambient context. *)
let ctx_k : Logs.Tag.t Str_map.t LS.t = LS.create ()

let create_tag ?doc name pp : _ tag = Logs.Tag.def ?doc name pp

let get_tags_from_ctx () : Logs.Tag.set =
  let map = LS.get ~default:Str_map.empty ctx_k in
  (* build the current set of tags *)
  Str_map.fold
    (fun _ (Logs.Tag.V (tag, v)) set -> Logs.Tag.add tag v set)
    map Logs.Tag.empty

let with_tag (tag : _ tag) v (f : unit -> 'b) : 'b =
  let old_map = LS.get ~default:Str_map.empty ctx_k in
  let new_map = Str_map.add (Logs.Tag.name tag) (Logs.Tag.V (tag, v)) old_map in
  LS.with_value ctx_k new_map f
