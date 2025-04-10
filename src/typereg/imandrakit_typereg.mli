(** Type registry.

    The registry maps path+type name to a concise description of the type's
    definition, which can be useful to generate documentation, JSON schema,
    check version compatibility, etc. *)

module Ty_expr = Ty_expr
module Ty_def = Ty_def

type t
(** Registry of definitions *)

val create : unit -> t

val top : t
(** Toplevel (default) register *)

val declare : t -> __FILE__:string -> Ty_def.t list -> unit
(** [declare reg ~__FILE__ tys] declares a clique of types to the registry, with
    these types being declared in [__FILE__].
    @raise Failure if some types are already registered. *)

val to_iter : t -> Ty_def.clique Iter.t

val find :
  t -> path:string -> name:string -> unit -> (Ty_def.t * Ty_def.clique) option

val find_exn :
  t -> path:string -> name:string -> unit -> Ty_def.t * Ty_def.clique
(** Same as {!find} but:
    @raise Not_found if not present *)
