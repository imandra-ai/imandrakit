module LS = Moonpool.Task_local_storage

type 'a tag = 'a Logs.Tag.def

(** Storage key for the ambient context. *)
let ctx_k : Logs.Tag.t Str_map.t Hmap.key = Hmap.Key.create ()

let create_tag ?doc name pp : _ tag = Logs.Tag.def ?doc name pp

let get_tags_from_ctx () : Logs.Tag.set =
  let map =
    try LS.get_in_local_hmap_opt ctx_k |> Option.value ~default:Str_map.empty
    with _ -> Str_map.empty (* might be running outside of a fiber/task *)
  in
  (* build the current set of tags *)
  Str_map.fold
    (fun _ (Logs.Tag.V (tag, v)) set -> Logs.Tag.add tag v set)
    map Logs.Tag.empty

let with_tag (tag : _ tag) v (f : unit -> 'b) : 'b =
  match
    LS.get_in_local_hmap_opt ctx_k |> Option.value ~default:Str_map.empty
  with
  | exception _ -> f ()
  | old_map ->
    let new_map =
      Str_map.add (Logs.Tag.name tag) (Logs.Tag.V (tag, v)) old_map
    in
    LS.set_in_local_hmap ctx_k new_map;
    Fun.protect ~finally:(fun () -> LS.set_in_local_hmap ctx_k old_map) f
