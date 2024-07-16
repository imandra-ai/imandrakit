module Tbl = Str_tbl
module Ty_expr = Ty_expr
module Ty_def = Ty_def

type t = {
  tbl: (Ty_def.t * Ty_def.clique) Tbl.t;
  mutable cliques: Ty_def.clique list;
}

let[@inline] key_ ~path ~name : string = spf "%s$%s" path name
let[@inline] key_of_d_ (d : Ty_def.t) : string = key_ ~path:d.path ~name:d.name
let create () : t = { tbl = Tbl.create 8; cliques = [] }
let top = create ()

let declare (self : t) ~__FILE__ (clique : Ty_def.clique) : unit =
  self.cliques <- clique :: self.cliques;
  List.iter
    (fun d ->
      let key = key_of_d_ d in
      if Tbl.mem self.tbl key then
        failwith
          (spf
             "typereg collision in '%s': type def %S in %S is already present."
             __FILE__ d.name d.path);
      Tbl.add self.tbl key (d, clique))
    clique

let[@inline] to_iter self k : unit = List.iter k self.cliques
let[@inline] find self ~path ~name () = Tbl.get self.tbl (key_ ~path ~name)
let[@inline] find_exn self ~path ~name () = Tbl.find self.tbl (key_ ~path ~name)

(*
(** Precise type-based versioning

    Versions are computed using a registry of type definitions,
    from which hashes can be computed *)
*)
