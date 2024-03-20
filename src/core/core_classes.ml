(** Some fundamental base classes *)

(** Named object *)
class type named =
  object
    method name : string
    (** Name of this thing. *)
  end

class virtual named' =
  object
    method virtual name : string
  end

(* subtyping test *)
let () = ignore (fun (_x : named') : named -> (_x :> named))

(** Statistics *)
class type with_stats =
  object
    method add_stats : Stats.t -> unit
    (** Add statistics. *)
  end

(** Default class for {!with_stats} *)
class with_stats' : with_stats =
  object
    method add_stats (_ : Stats.t) = ()
  end
