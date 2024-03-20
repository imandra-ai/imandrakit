(** Basic hashconsing. *)

(** Necessary arguments for hashconsing. *)
module type ARG = sig
  type t

  val equal : t -> t -> bool
  (** Structural equality *)

  val hash : t -> int
  (** Structural hash *)

  val set_id : t -> int -> unit
  (** Change the ID of this hashconsed object. The ID must be
      uninitialized; this will only be called on uninitialized (new) objects. *)
end

(** Create a new hashconsed type. *)
module Make (A : ARG) : sig
  type t

  val create : ?size:int -> unit -> t

  val hashcons : t -> A.t -> A.t

  val size : t -> int

  val remove : t -> A.t -> unit

  val to_iter : t -> A.t Iter.t
end = struct
  module W = Weak.Make (A)

  type t = {
    tbl: W.t;
    mutable n: int;
  }

  let create ?(size = 1_024) () = { tbl = W.create size; n = 0 }

  let size self = W.count self.tbl

  (* hashcons terms *)
  let hashcons st t =
    let t' = W.merge st.tbl t in
    if t == t' then (
      A.set_id t' st.n;
      st.n <- 1 + st.n
    );
    t'

  let[@inline] remove st u : unit = W.remove st.tbl u

  let to_iter st yield = W.iter yield st.tbl
end
