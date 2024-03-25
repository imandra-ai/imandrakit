(** Extensible data for errors *)

module Ser = Imandrakit_ser

module Key : sig
  type 'a t

  val make : field_name:string -> codec:'a Ser.Codec.t -> unit -> 'a t

  type any = Any : _ t -> any

  val find : string -> any option
end

type 'a key = 'a Key.t
type t

val empty : t
val add : 'a Key.t -> 'a -> t -> t
val is_empty : t -> bool
val get : 'a Key.t -> t -> 'a option
val codec : t Ser.Codec.t
