open Types
module Immediate = Immediate

type t

val create : unit -> t
(** New encoder. *)

val reset : t -> unit
(** Reset the encoder. Previous slices obtained via {!finalize}
are invalidated. *)

type 'a encoder = t -> 'a -> offset

val ignore_offset : offset -> unit
(** Used when we don't use the resulting offset *)

(** {2 Primitives} *)

val immediate : immediate encoder
val null : unit encoder
val bool : bool encoder
val int : int encoder
val int64 : int64 encoder
val float32 : float encoder
val float : float encoder
val string : string encoder
val string_slice : slice encoder
val blob : string encoder
val blob_slice : slice encoder

(* TODO: ? val key : unit encoder *)
val tag : t -> tag:int -> v:offset -> offset
val pointer : int encoder

(** {2 Arrays} *)

val array_init : t -> int -> (int -> immediate) -> offset
val array : t -> immediate array -> offset
val array_iter : t -> immediate Iter.t -> offset
val list : t -> immediate list -> offset

(** {2 Dictionaries} *)

val dict : t -> int -> (int -> immediate * immediate) -> offset
val dict_iter : t -> (immediate * immediate) Iter.t -> offset
val dict_list : t -> (immediate * immediate) list -> offset

(** {2 Sum types} *)

val cstor : t -> index:int -> immediate array -> offset

(** {2 Top value} *)

val finalize : t -> top:offset -> slice
(** Finalize by writing the entrypoint, and get the result as a byte slice
 @param top the offset of the toplevel/entrypoint  value *)

val finalize_copy : t -> top:offset -> string
(** Finalize, and get a copy of the result. See {!finalize} *)

val to_string : 'a encoder -> 'a -> string
(** Full entrypoint *)

(*
val record_descriptor :
  encoder -> key:string -> f:(unit -> record_descriptor) -> unit -> offset
(** [record_descriptor enc name ~f] adds or returns the offset for
        the unique descriptor for record type [name]. *)

val record : encoder -> descr:offset -> unit -> offset

val sum_type_descriptor :
  encoder -> key:string -> f:(unit -> sum_type_descriptor) -> unit -> offset
val cstor : encoder -> descr:offset -> index:int -> unit -> offset
  *)
