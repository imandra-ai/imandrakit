(** Extensible data for errors.

    Errors can contain arbitrary metadata in each level of its
    context, ie each message (as long as the metadata
    is serializable and printable).
*)

module Ser_pack = Imandrakit_ser_pack

module Key : sig
  type 'a t
  (** Key to get/set errors of type ['a]. Generative. *)

  val make :
    field_name:string ->
    enc:'a Ser_pack.Ser.t ->
    dec:'a Ser_pack.Deser.t ->
    pp:'a Fmt.printer ->
    unit ->
    'a t
  (** Generate a new key. Each call will produce a new distinct key. *)

  type any = Any : _ t -> any

  val find : string -> any option
  val pp : 'a t -> 'a Fmt.printer
end

type 'a key = 'a Key.t
type t [@@deriving serpack]

val empty : t
(** Empty set of data *)

val add : 'a Key.t -> 'a -> t -> t
(** Add data of type ['a] to the set *)

val is_empty : t -> bool
(** Is there nothing in the set? *)

val get : 'a Key.t -> t -> 'a option
(** Get data of type ['a] using this key. *)

type binding = B : 'a Key.t * 'a -> binding

val iter : t -> binding Iter.t
