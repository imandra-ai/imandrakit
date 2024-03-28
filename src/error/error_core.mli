(** Universal error type for Imandra.

    Exposes the standard errors that Imandra can raise. *)

type message = {
  msg: string;
  data: Data.t;
  bt: string option;  (** Backtrace *)
}
[@@deriving serpack]
(** A message.

    An error message is emitted at a particular place in the code.
    An error can contain several error messages. *)

type stack = message list

type t = {
  process: string;
  kind: Kind.t;
  msg: message;
  stack: stack;
}
[@@deriving show, serpack]

exception E of t
(** Internal error *)

val pp_with : show_process:bool -> t Fmt.printer

module Message : sig
  type t = message [@@deriving show]

  val data : t -> Data.t
  val get : 'a Data.key -> t -> 'a option
end

val data : t -> Data.t
val get_data : 'a Data.key -> t -> 'a option
val raise_err : ?bt:Printexc.raw_backtrace -> t -> 'a
val add_bt : string -> t -> t
val add_ctx : message -> t -> t
val add_data : 'a Data.key -> 'a -> t -> t

val guard : ?let_pass:(exn -> bool) -> (unit -> message) -> (unit -> 'a) -> 'a
(** [guard g f] behaves like [f()], excepts that if [f()]
    raises [Error e], [guard g f] raises [Error e']
    where [e'] wraps [e] with context error [g()].
    @param let_pass if it returns [true] for an exception, the exception is
      re-raised. *)

type !'a result = ('a, t) Stdlib.result [@@deriving show, map, iter]

val unwrap : 'a result -> 'a
(** [unwrap e] uses {!raise_err} to unpack the result *)

module Infix : sig
  val ( let*! ) : (unit -> message) -> (unit -> 'a) -> 'a
  (** Similar to {!guard} *)
end

include module type of Infix

(**/**)

module Internal_ : sig
  val get_process_name : unit -> string
end

(**/**)
