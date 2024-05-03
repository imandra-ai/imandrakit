open Types

type encoder

val create : unit -> encoder
val finalize : encoder -> Slice.t

type 'a t = encoder -> 'a -> offset
type 'a t' = encoder -> 'a -> unit

val ignore_offset : offset -> unit
(** Used when we don't use the resulting offset *)

val null : unit t
val bool : bool t
val int : int t
val int64 : int64 t
val float32 : float t
val float : float t
val array_start : unit t
val array_start' : unit t'
val array_stop : unit t'
val string : string t
val blob : string t
val key : unit t
val pointer : int t
val dict_start : unit t
val dict_start' : unit t'
val dict_stop : unit t'

val record_descriptor :
  encoder -> key:string -> f:(unit -> record_descriptor) -> unit -> offset
(** [record_descriptor enc name ~f] adds or returns the offset for
        the unique descriptor for record type [name]. *)

val record : encoder -> descr:offset -> unit -> offset

val sum_type_descriptor :
  encoder -> key:string -> f:(unit -> sum_type_descriptor) -> unit -> offset

val cstor : encoder -> descr:offset -> index:int -> unit -> offset
