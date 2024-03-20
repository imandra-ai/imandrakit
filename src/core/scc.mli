(** Strongly connected components. *)

module type ARG = sig
  type t
  type node

  val children : t -> node -> node Iter.t

  module Node_tbl : Hashtbl.S with type key = node
end

module type S = sig
  module A : ARG

  val sccs : graph:A.t -> nodes:A.node list -> A.node list A.Node_tbl.t
end

module Make (A : ARG) : S with module A = A
