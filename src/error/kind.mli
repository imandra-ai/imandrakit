(** Error kinds *)

type t = private { name: string } [@@unboxed] [@@deriving show, eq, ord]
(** The kind of an error, ie a category the error belongs to *)

val make : name:string -> unit -> t
val codec : t Codec.t

(** {2 Some standard error kinds} *)

val generic_internal_error : t
(** Any internal error that's not more specific *)

val timeout : t
(** Timeout *)
