(** Decoders for {!Ser_value}.

    Combinators to decode values. *)

(** Errors *)
module Error : sig
  type t [@@deriving show]

  val of_string : string -> Value.t -> t
end

(** {2 Main combinators *)

type +'a t
(** Decode a value of type ['a] *)

val int : int t
val bool : bool t
val string : string t
val return : 'a -> 'a t
val return_result : ('a, string) result -> 'a t
val return_result_err : ('a, Error.t) result -> 'a t
val delay : (unit -> 'a t) -> 'a t
val fail : string -> 'a t
val failf : ('a, Format.formatter, unit, 'b t) format4 -> 'a
val fail_err : Error.t -> 'a t

val unwrap_opt : string -> 'a option -> 'a t
(** Unwrap option, or fail *)

val any : Value.t t
val list : 'a t -> 'a list t
val tup2 : 'a t -> 'b t -> ('a * 'b) t
val tup3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val tup4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
val dict_field : string -> 'a t -> 'a t
val dict_field_opt : string -> 'a t -> 'a option t
val dict_field_or : 'a -> string -> 'a t -> 'a t
val dict : Value.t Str_map.t t
val dict_as_list : (string * Value.t) list t
val both : 'a t -> 'b t -> ('a * 'b) t

val reflect : 'a t -> Value.t -> ('a, Error.t) result t
(** [reflect dec v] returns the result of decoding [v] with [dec] *)

val apply : 'a t -> Value.t -> 'a t
(** [apply dec v] uses [dec] to decode [v] *)

val try_l : 'a t list -> 'a t
(** [try_l fs] tries each [f in fs] turn by turn, until one succeeds *)

val map_l : ('a -> 'b t) -> 'a list -> 'b list t
val fold_l : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t

module Infix : sig
  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
end

include module type of Infix

(** {2 Deserializing} *)

val run : 'a t -> Value.t -> ('a, Error.t) result

exception Fail of Error.t

val run_exn : 'a t -> Value.t -> 'a
(** @raise Fail in case of failure *)
