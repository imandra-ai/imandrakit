(** Logging context.

    This module provides ambient-context tags that will be
    automatically accounted for in {!Logger}.
*)

type 'a tag = 'a Logs.Tag.def

val create_tag : ?doc:string -> string -> 'a Fmt.printer -> 'a tag

val set_tag : 'a tag -> 'a -> unit
(** Set tag for the rest of the current task, does nothing outside of a task *)

val with_tag : 'a tag -> 'a -> (unit -> 'b) -> 'b
(** Set the tag to this value locally in the ambient context *)

val get_tags_from_ctx : unit -> Logs.Tag.set
