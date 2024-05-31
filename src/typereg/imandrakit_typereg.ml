module Tbl = Str_tbl
module Ty_expr = Ty_expr
module Ty_def = Ty_def

type t = { tbl: Ty_def.t Tbl.t } [@@unboxed]

let[@inline] key_ ~path ~name : string = spf "%s$%s" path name
let[@inline] key_of_d_ (d : Ty_def.t) : string = key_ ~path:d.path ~name:d.name
let create () : t = { tbl = Tbl.create 8 }
let top = create ()

let declare (self : t) ~__FILE__ (decls : Ty_def.t list) : unit =
  List.iter
    (fun d ->
      let key = key_of_d_ d in
      if Tbl.mem self.tbl key then
        failwith
          (spf
             "typereg collision in '%s': type def %S in %S is already present."
             __FILE__ d.name d.path);
      Tbl.add self.tbl key d)
    decls

let[@inline] to_iter self k : unit = Tbl.iter (fun _ d -> k d) self.tbl
let[@inline] find self ~path ~name () = Tbl.get self.tbl (key_ ~path ~name)
let[@inline] find_exn self ~path ~name () = Tbl.find self.tbl (key_ ~path ~name)

(*
(** Precise type-based versioning

    Versions are computed using a registry of type definitions,
    from which hashes can be computed *)
*)
