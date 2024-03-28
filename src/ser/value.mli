(** Serialization representation.

    A [Ser_value.t] describes how to serialized some structured
    data into bytes.
    It reflects the shape of the structured data but does not commit to a
    particular serialization format.
*)

type t = private
  | Null
  | Bool of bool
  | Str of string
  | Bytes of string
  | Int of int64
  | Float of float
  | List of t list
  | Dict of t Str_map.t
  | Tag of int * t
[@@deriving show, eq]

(** {2 Helpers} *)

val is_null : t -> bool
val hash : t -> int

(** {2 Constructors} *)

val null : t
val bool : bool -> t
val int : int -> t
val int64 : int64 -> t
val float : float -> t
val string : string -> t
val bytes : string -> t
val list : t list -> t
val dict : t Str_map.t -> t
val dict_of_list : (string * t) list -> t
val tag : int -> t -> t
