(** Universal error type for Imandrakit. *)

type message = {
  msg: string;
  bt: string option;  (** Backtrace *)
}
[@@deriving twine]
(** A message.

    An error message is emitted at a particular place in the code. An error can
    contain several error messages. *)

type stack = message list

type +'a t = {
  err: 'a;
  process: string;
  data: Data.t;
  stack: stack;
}
[@@deriving show, twine]

type ('a, 'err) or_error = ('a, 'err t) Stdlib.Result.t
[@@deriving show, eq, twine]

type 'a ectx

val with_ectx : ('err ectx -> 'a) -> ('a, 'err) or_error
(** Local try/catch mechanism *)

val raise_err : ?bt:Printexc.raw_backtrace -> 'err ectx -> 'err t -> 'a
val unwrap : 'err ectx -> ('a, 'err) or_error -> 'a
val pp_with : show_process:bool -> 'err Fmt.printer -> 'err t Fmt.printer

module Message : sig
  type t = message [@@deriving show]

  val data : t -> Data.t
  val get : 'a Data.key -> t -> 'a option
end

type msg_t = [ `Msg of string ] t [@@deriving show, twine]
(** Error with a message *)

exception E of msg_t

val data : _ t -> Data.t
val get_data : 'a Data.key -> _ t -> 'a option
val add_bt : string -> 'err t -> 'err t
val add_ctx : message -> 'err t -> 'err t
val add_data : 'a Data.key -> 'a -> 'err t -> 'err t
val raise_msg_err : ?bt:Printexc.raw_backtrace -> msg_t -> 'a

(* TODO:
val guard : ?let_pass:(exn -> bool) -> (unit -> 'err ) -> (unit -> 'a) -> 'a
(** [guard g f] behaves like [f()], excepts that if [f()] raises [Error e],
    [guard g f] raises [Error e'] where [e'] wraps [e] with context error [g()].
    @param let_pass
      if it returns [true] for an exception, the exception is re-raised. *)

*)

module Infix : sig
  val ( let*! ) : (unit -> message) -> (unit -> 'a) -> 'a
  (** Similar to {!guard} *)
end

include module type of Infix
