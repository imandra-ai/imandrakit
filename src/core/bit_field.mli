(** Efficient Bit Field for up to 31 or 61 fields.

    This module defines efficient bitfields
    up to 31 or 61 bits (depending on the architecture) in
    a relatively type-safe way.

    {[
      module B = CCBitField.Make(struct end);;

      let x = B.mk_field ()
      let y = B.mk_field ()
      let z = B.mk_field ()

      let f = B.empty |> B.set x true |> B.set y true;;

      assert (not (B.get z f)) ;;

      assert (f |> B.set z true |> B.get z);;

    ]}
*)

exception TooManyFields
(** Raised when too many fields are packed into one bitfield. *)

exception Frozen
(** Raised when a frozen bitfield is modified. *)

val max_width : int
(** System-dependent maximum width for a bitfield, typically 30 or 62. *)

(** Bitfield Signature *)
module type S = sig
  type t = private int
  (** Generative type of bitfields. Each instantiation of the functor
      should create a new, incompatible type *)

  val empty : t
  (** Empty bitfields (all bits 0). *)

  val ( lor ) : t -> t -> t

  val ( land ) : t -> t -> t

  type field

  val get : field -> t -> bool
  (** Get the value of this field. *)

  val set : field -> bool -> t -> t
  (** Set the value of this field. *)

  val mk_field : unit -> field
  (** Make a new field. *)

  val freeze : unit -> unit
  (** Prevent new fields from being added. From now on, creating
      a field will raise Frozen. *)

  val total_width : unit -> int
  (** Current width of the bitfield. *)

  val unsafe_of_int : int -> t
end

module Make () : S
(** Create a new bitfield type *)

(**/**)

val all_bits_ : int -> int -> int

(**/**)
