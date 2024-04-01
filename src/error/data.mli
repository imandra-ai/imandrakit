(** Extensible data for errors *)

module Ser_pack = Imandrakit_ser_pack

module Key : sig
  type 'a t

  val make :
    field_name:string ->
    enc:'a Ser_pack.Ser.t ->
    dec:'a Ser_pack.Deser.t ->
    pp:'a Fmt.printer ->
    unit ->
    'a t

  type any = Any : _ t -> any

  val find : string -> any option
  val pp : 'a t -> 'a Fmt.printer
end

type 'a key = 'a Key.t
type t [@@deriving serpack]

val empty : t
val add : 'a Key.t -> 'a -> t -> t
val is_empty : t -> bool
val get : 'a Key.t -> t -> 'a option

type binding = B : 'a Key.t * 'a -> binding

val iter : t -> binding Iter.t
