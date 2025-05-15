open Types

type t

val of_slice : slice -> t
val of_string : string -> t
val of_in_channel : in_channel -> t
val hmap_set : t -> 'a Hmap.key -> 'a -> unit
val hmap_get : t -> 'a Hmap.key -> 'a option

val hmap_transfer : t -> into:t -> unit
(** Transfer all hmap pairs from the first decode to the second *)

type 'a decoder = t -> offset -> 'a
type cstor_index = int

val fail : string -> 'a
val failf : ('a, unit, string, 'b) format4 -> 'a

module Value_kind : sig
  type t =
    | Special
    | Int
    | Float
    | String
    | Blob
    | Ref
    | Pointer
    | Array
    | Dict
    | Tag
    | Cstor0
    | Cstor
    | Invalid
  [@@deriving eq, show { with_path = false }]
end

module Value : sig
  type array_cursor
  type dict_cursor

  (** A value *)
  type t =
    | Null
    | True
    | False
    | Int of int64
    | Float of float
    | String of slice
    | Blob of slice
    | Ref of offset  (** Explicit reference *)
    | Pointer of offset  (** Implicit reference, followed implicitly *)
    | Array of array_cursor
    | Dict of dict_cursor
    | Tag of int * offset
    | Cstor0 of cstor_index
    | CstorN of cstor_index * array_cursor
  [@@deriving show { with_path = false }]
end

module Array_cursor : sig
  type t = Value.array_cursor [@@deriving show]

  val length : t -> int
  (** Number of items *)

  val current : t -> offset
  (** Current offset in the cursor *)

  val consume : t -> unit
  (** Skip current offset, go to the next one *)

  val get_value_and_consume : t -> Value.t
  val to_iter : t -> Value.t Iter.t
  val to_array_of : (offset -> 'a) -> t -> 'a array
  val to_iter_of : t -> (offset -> 'a) -> 'a Iter.t
  val to_list : t -> Value.t list
  val to_list_of : (offset -> 'a) -> t -> 'a list
end

module Dict_cursor : sig
  type t = Value.dict_cursor [@@deriving show]

  val length : t -> int
  val current : t -> offset * offset
  val consume : t -> unit
  val get_key_value_and_consume : t -> Value.t * Value.t
  val to_iter : t -> (Value.t * Value.t) Iter.t
  val to_iter_of : t -> (offset -> offset -> 'a) -> 'a Iter.t
  val to_array_of : (offset -> offset -> 'a) -> t -> 'a array
  val to_list : t -> (Value.t * Value.t) list
  val to_list_of : (offset -> offset -> 'a) -> t -> 'a list
end

val deref_rec : offset decoder
(** Given any value, follow pointers until a non-pointer value is reached, a
    return its address. *)

val read : ?auto_deref:bool -> Value.t decoder
(** Read a value of any kind.
    @param auto_deref if true (default), follow pointers implicitly *)

val get_value_kind : ?auto_deref:bool -> Value_kind.t decoder
(** Read the kind of the value at given offset *)

val null : unit decoder
val bool : bool decoder
val int_truncate : int decoder
val int64 : int64 decoder
val float : float decoder
val string_slice : slice decoder
val string : string decoder
val blob_slice : slice decoder
val blob : string decoder
val array : Array_cursor.t decoder
val list_of : 'a decoder -> 'a list decoder
val dict : Dict_cursor.t decoder
val tag : (int * offset) decoder
val cstor : (cstor_index * Array_cursor.t) decoder

val ref_ : offset decoder
(** Read a reference *)

val ref_for : 'a offset_for decoder
(** Read a typed reference (same as a reference) *)

(** {2 Entrypoint} *)

val get_entrypoint : t -> offset
(** Offset of the entrypoint (the topevel value) *)

val read_entrypoint : t -> Value.t
(** Read the entrypoint, from the end of the slice *)

val read_ref : t -> 'a decoder -> 'a offset_for -> 'a
(** Decode an explicit reference *)

val decode_string : ?init:(t -> unit) -> 'a decoder -> string -> 'a

(** {2 Caching}

    Caching is used to reflect the sharing of values embedded in a Twine slice,
    into the decoded values. It means that, for a given type, if values of this
    type are encoded with sharing (e.g. a graph-heavy term representation), then
    with caching we can decode the values to OCaml values that also have
    sharing. *)

type 'a cache_key
(** Generative key used to cache values during decoding *)

val create_cache_key : unit -> _ cache_key
(** Generate a new (generative) cache key for a type.

    {b NOTE} this should be called only at module toplevel, as a constant, not
    dynamically inside a function:
    [let key: foo value_pack.Deser.cache_key = value_pack.Deser.create_cache_key
     ();;]. Indeed, this is generative, so creating multiple keys for a type
    will result in sub-par performance or non-existent caching. *)

val with_cache : 'a cache_key -> 'a decoder -> 'a decoder
(** [with_cache key dec] is the same decoder as [dec] but it uses [key] to
    retrieve values directly from an internal table for entries/values that have
    already been decoded in the past. This means that a value that was encoded
    with a lot of sharing (e.g in a graph, or a large string using
    {!Ser.add_string}) will be decoded only once. *)

val add_cache : 'a decoder ref -> unit
(** [add_cache dec_ref] modifies the decoder so it uses a new cache key. It is
    the same as:
    {[
      let key = create_cache_key ()
      let () = dec_ref := with_cache key !dec_ref
    ]} *)
