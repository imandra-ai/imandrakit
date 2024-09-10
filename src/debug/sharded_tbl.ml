(** A concurrent sharded hashmap *)

module type S = sig
  type key
  type 'a t

  val create : ?size_each:int -> unit -> _ t
  val with_add_or_create : 'a t -> key -> create:(key -> 'a) -> ('a -> 'b) -> 'b
  val find : 'a t -> key -> 'a option
  val with_find : 'a t -> key -> ('a option -> 'b) -> 'b
  val clear : _ t -> unit
  val remove : _ t -> key -> unit
  val iter : 'a t -> (key -> 'a -> unit) -> unit

  val find_exn : 'a t -> key -> 'a
  (** @raise Not_found *)
end

open struct
  module type SHARD = sig
    val n_shards : int
    val mask : int
  end
end

module Make_ (Shard : SHARD) (K : Hashtbl.HashedType) : S with type key = K.t =
struct
  open Shard
  module Tbl = Hashtbl.Make (K)

  type key = K.t

  (* TODO: use https://ocaml-multicore.github.io/picos/doc/picos_aux/Picos_aux_htbl/index.html
     for each shard table *)

  type 'a t = { shards: 'a Tbl.t Lock.t array  (** An array of tables *) }
  [@@unboxed]

  let create ?(size_each = 8) () : _ t =
    {
      shards =
        Array.init n_shards (fun _ -> Lock.create @@ Tbl.create size_each);
    }

  let clear self =
    Array.iter (fun tbl -> Lock.with_lock tbl Tbl.clear) self.shards

  let iter self f =
    Array.iter
      (fun tbl -> Lock.with_lock tbl (fun tbl -> Tbl.iter f tbl))
      self.shards

  let remove self k =
    let h = K.hash k land mask in
    let tbl = Array.get self.shards h in
    Lock.with_lock tbl (fun tbl -> Tbl.remove tbl k)

  let find_exn (self : 'a t) k : 'a =
    let h = K.hash k land mask in
    let tbl = Array.get self.shards h in
    Lock.with_lock tbl (fun tbl -> Tbl.find tbl k)

  let find (self : 'a t) k : 'a option =
    let h = K.hash k land mask in
    let tbl = Array.get self.shards h in
    Lock.with_lock tbl (fun tbl -> Tbl.find_opt tbl k)

  let with_find self k f =
    let h = K.hash k land mask in
    let tbl = Array.get self.shards h in
    Lock.with_lock tbl (fun tbl -> f (Tbl.find_opt tbl k))

  let with_add_or_create (self : _ t) k ~create f =
    let h = K.hash k land mask in
    let tbl = Array.get self.shards h in
    Lock.with_lock tbl (fun tbl ->
        match Tbl.find tbl k with
        | v -> f v
        | exception Not_found ->
          let v = create k in
          Tbl.add tbl k v;
          f v)
end

module Make8 = Make_ (struct
  let n_shards = 8
  let mask = 0b111
end)

module Make16 = Make_ (struct
  let n_shards = 16
  let mask = 0b1111
end)
