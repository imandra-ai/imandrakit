module LS = Moonpool.Task_local_storage

type 'a tag = 'a Logs.Tag.def

(** Storage key for the ambient context. *)
let ctx_k : Logs.Tag.t Str_map.t LS.key =
  LS.new_key ~init:(fun () -> Str_map.empty) ()

let create_tag ?doc name pp : _ tag = Logs.Tag.def ?doc name pp

let get_tags_from_ctx () : Logs.Tag.set =
  match LS.get ctx_k with
  | exception Failure _ -> Logs.Tag.empty
  | map ->
    (* build the current set of tags *)
    Str_map.fold
      (fun _ (Logs.Tag.V (tag, v)) set -> Logs.Tag.add tag v set)
      map Logs.Tag.empty

let with_tag (tag : _ tag) v (f : unit -> 'b) : 'b =
  match LS.get ctx_k with
  | exception Failure _ -> f ()
  | old_map ->
    let new_map =
      Str_map.add (Logs.Tag.name tag) (Logs.Tag.V (tag, v)) old_map
    in
    LS.with_value ctx_k new_map f
