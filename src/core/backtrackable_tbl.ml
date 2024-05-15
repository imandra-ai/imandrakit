module type S = sig
  type key
  type 'a t

  val create : ?size:int -> unit -> 'a t

  val find : 'a t -> key -> 'a
  (** @raise Not_found if the key is not present *)

  val get : 'a t -> key -> 'a option
  val get_or : default:'a -> 'a t -> key -> 'a
  val get_or_add : 'a t -> f:(key -> 'a) -> k:key -> 'a
  val update : 'a t -> f:(key -> 'a option -> 'a option) -> k:key -> unit
  val mem : _ t -> key -> bool
  val length : _ t -> int
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val to_iter : 'a t -> (key * 'a) Iter.t
  val add : 'a t -> key -> 'a -> unit
  val remove : _ t -> key -> unit
  val n_levels : _ t -> int
  val push_level : _ t -> unit
  val pop_levels : 'a t -> int -> unit
  val pop_levels_and_keep : 'a t -> int -> unit
end

module type ARG = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
end

module Make (A : ARG) = struct
  type key = A.t

  module M = CCHashtbl.Make (A)
  module BS = Backtrack_stack

  type 'a undo_op =
    | Add of key * 'a
    | Remove of key

  type 'a t = {
    tbl: 'a M.t;
    undo: 'a undo_op BS.t;
  }

  let create ?(size = 32) () : _ t =
    { tbl = M.create size; undo = BS.create () }

  let apply_undo self u =
    match u with
    | Add (k, v) -> M.replace self.tbl k v
    | Remove k -> M.remove self.tbl k

  let[@inline] find (self : _ t) k = M.find self.tbl k
  let[@inline] get (self : _ t) k : _ option = M.get self.tbl k
  let[@inline] get_or ~default (self : _ t) k = M.get_or ~default self.tbl k
  let[@inline] mem self k = M.mem self.tbl k
  let[@inline] length self = M.length self.tbl
  let[@inline] iter f self = M.iter f self.tbl
  let[@inline] push_level self = BS.push_level self.undo

  let[@inline] pop_levels self n =
    BS.pop_levels self.undo n ~f:(apply_undo self)

  let[@inline] pop_levels_and_keep self n = BS.pop_levels self.undo n ~f:ignore
  let[@inline] n_levels self = BS.n_levels self.undo

  let add self k v : unit =
    if BS.n_levels self.undo > 0 then (
      try
        let old_v = M.find self.tbl k in
        BS.push self.undo (Add (k, old_v))
      with Not_found -> BS.push self.undo (Remove k)
    );
    M.replace self.tbl k v

  let remove self k : unit =
    if BS.n_levels self.undo > 0 then (
      try
        (* get value to restore it *)
        let v = M.find self.tbl k in
        M.remove self.tbl k;
        BS.push self.undo (Add (k, v))
      with Not_found -> ()
    ) else
      M.remove self.tbl k

  let get_or_add tbl ~f ~k =
    try find tbl k
    with Not_found ->
      let v = f k in
      add tbl k v;
      v

  let update tbl ~f ~k =
    let v = get tbl k in
    match v, f k v with
    | None, None -> ()
    | Some _, None -> remove tbl k
    | _, Some v' -> add tbl k v'

  let[@inline] to_iter self yield = M.iter (fun k v -> yield (k, v)) self.tbl
end
