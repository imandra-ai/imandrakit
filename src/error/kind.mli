(** Error kinds *)

type t = private { name: string }
[@@unboxed] [@@deriving show, eq, ord, serpack]
(** The kind of an error, ie a category the error belongs to *)

val make : name:string -> unit -> t

(** {2 Some standard error kinds} *)

val generic_internal_error : t
(** Any internal error that's not more specific *)

val timeout : t
(** Timeout *)
