(** {1 A backtrackable hashtable} *)

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
end

module type ARG = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
end

module Make (A : ARG) : S with type key = A.t
