module type S = sig
  type t_from

  type t_into

  type t

  val create : int -> t

  val length : t -> int

  val add : t -> t_from -> t_into -> unit

  val mem_from : t -> t_from -> bool

  val get_from : t -> t_from -> t_into option

  val mem_into : t -> t_into -> bool

  val get_into : t -> t_into -> t_from option

  val remove : t -> t_from -> unit
end

module type ARG = sig
  module From : Hashtbl.HashedType

  module Into : Hashtbl.HashedType
end

module Make (A : ARG) :
  S with type t_from = A.From.t and type t_into = A.Into.t = struct
  type t_from = A.From.t

  type t_into = A.Into.t

  module H1 = Hashtbl.Make (A.From)
  module H2 = Hashtbl.Make (A.Into)

  type t = {
    h1: t_into H1.t;
    h2: t_from H2.t;
  }

  let create n : t = { h1 = H1.create n; h2 = H2.create n }

  let length self : int = H1.length self.h1

  let add self k1 k2 : unit =
    H1.replace self.h1 k1 k2;
    H2.replace self.h2 k2 k1

  let mem_from self k : bool = H1.mem self.h1 k

  let get_from self k : _ option = H1.find_opt self.h1 k

  let mem_into self k : bool = H2.mem self.h2 k

  let get_into self k : _ option = H2.find_opt self.h2 k

  let remove self k1 =
    match H1.find_opt self.h1 k1 with
    | Some k2 ->
      H1.remove self.h1 k1;
      H2.remove self.h2 k2
    | None -> ()
end
