(** LRU cache.

    Fixed size cache that evicts the least-recently-used entry when it's full.
    This structure is thread-safe. *)

module type S = sig
  type key
  type 'v t

  val create : max_size:int -> unit -> _ t
  val size : _ t -> int
  val clear : _ t -> unit
  val get : 'v t -> key -> 'v option
  val set : 'v t -> key -> 'v -> unit
end

module Make (K : Hashtbl.HashedType) : S with type key = K.t = struct
  type key = K.t

  module Tbl = Hashtbl.Make (K)

  type 'v node = {
    k: key;
    mutable v: 'v;
    mutable next: 'v node;
        (** Double linked list of nodes. Invariant: the list formed of [.next]
            pointers is sorted so that the earliest elements in the list are the
            ones that have been accessed most recently. *)
    mutable prev: 'v node;
  }

  type 'v st = {
    max_size: int;
    mutable first: 'v node option;
    tbl: 'v node Tbl.t;
  }

  type 'v t = { st: 'v st Lock.t } [@@unboxed]

  let create ~max_size () : _ t =
    assert (max_size > 0);
    { st = Lock.create { max_size; tbl = Tbl.create 32; first = None } }

  let[@inline] size (self : _ t) = Tbl.length (Lock.get self.st).tbl

  let clear self =
    let@ self = Lock.with_lock self.st in
    Tbl.clear self.tbl;
    self.first <- None

  (** Move node [n] at the beginning of the list. *)
  let move_first_ (self : _ st) (n : _ node) =
    match self.first with
    | None ->
      n.next <- n;
      n.prev <- n;
      self.first <- Some n
    | Some n0 when n == n0 -> ()
    | Some n0 ->
      assert (n.next != n);
      (* remove [n] from list *)
      n.prev.next <- n.next;
      n.next.prev <- n.prev;
      (* insert [n] in front *)
      n.prev <- n0.prev;
      n.next <- n0;
      n0.prev.next <- n;
      n0.prev <- n;
      self.first <- Some n

  let get self k =
    let@ self = Lock.with_lock self.st in
    match Tbl.find self.tbl k with
    | n ->
      move_first_ self n;
      Some n.v
    | exception Not_found -> None

  let set (self : _ t) k v : unit =
    let@ self = Lock.with_lock self.st in
    assert (Tbl.length self.tbl <= self.max_size);
    match Tbl.find self.tbl k with
    | n ->
      n.v <- v;
      move_first_ self n
    | exception Not_found ->
      if Tbl.length self.tbl = self.max_size then (
        (* full, need to evict a node *)
          match self.first with
        | None -> assert false
        | Some n ->
          (* drop n.prev, hasn't been used for the longest time *)
          let prev = n.prev in
          prev.prev.next <- n;
          n.prev <- prev.prev;
          Tbl.remove self.tbl prev.k
      );

      let rec n = { k; v; next = n; prev = n } in
      Tbl.add self.tbl k n;
      (match self.first with
      | None -> self.first <- Some n
      | Some n0 ->
        (* insert [n] before [n0] *)
        n.next <- n0;
        n.prev <- n0.prev;
        n0.prev.next <- n;
        n0.prev <- n;
        self.first <- Some n)
end
