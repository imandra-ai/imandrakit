(** Error kinds.

 Each error has a single kind that is a concise description
 of the nature of the error. More details are typically provided
 in the error message. *)

type t = private { name: string }
[@@unboxed] [@@deriving show, eq, ord, serpack]
(** The kind of an error, ie a category the error belongs to *)

val hash : t -> int
val make : name:string -> unit -> t
val name : t -> string

(** {2 Some standard error kinds} *)

val generic_internal_error : t
(** Any internal error that's not more specific *)

val timeout : t
(** Timeout *)

val todo : t
(** Not implemented yet *)

module Tbl : CCHashtbl.S with type key = t
module Map : CCMap.S with type key = t
module Set : CCSet.S with type elt = t
