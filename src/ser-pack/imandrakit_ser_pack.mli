(** ser-pack.

  ser-pack is a data serialization scheme built on top of {!Imandrakit_ser}.
  It introduces a notion of {i heap} (an array of ser-values) and {i pointers} into
  this heap (ser-values consisting of an integer wrapped in a specific tag).
  The heap is then turned into an array of ser-values, so that the resulting "pack" is just a
  single large ser-value with a specific internal structure (some of which are pointers into
  the large internal {i heap}).

  When serializing a complex data structure that presents internal sharing (typically,
  with pointers/references), the heap can be used to represent that sharing directly
  in the ser-value. This is done by serializing a value once, adding it to the {i heap}
  (which is an array); the position of the ser-value in the heap can then be wrapped
  with tag 6 to become a {i pointer}. All other references to this value are serialized
  as pointers.
*)

type value = Imandrakit_ser.Value.t

exception Error of string

type nonrec 'a result = ('a, string) result

(** Serialization *)
module Ser : sig
  type state
  (** State used for serialization.

      It contains an in-progress heap, and a hash table for hashconsing. *)

  type 'a t = state -> 'a -> value
  (** Serializer for type ['a] *)

  val create : unit -> state
  (** New state. *)

  type ptr = value
  (** An integer + tag for value *)

  val unit : value
  (** Build a value null *)

  val int64 : int64 -> value
  (** Build a value integer *)

  val int : int -> value
  (** Build a value integer *)

  val bool : bool -> value
  (** Build a value bool *)

  val float : float -> value
  (** Build a value float *)

  val string : string -> value
  (** Build a value text string (UTF8) *)

  val bytes : string -> value
  (** Build a value blob (raw bytes) *)

  val list : value list -> value
  (** Build a value list *)

  val dict : value Str_map.t -> value

  val map : (string * value) list -> value
  (** Build a value map *)

  val list_of : 'a t -> 'a list t
  (** [list_of ser] encodes a list of values using [ser] for each *)

  val map_of : 'b t -> (string * 'b) list t
  (** Build a map by serializing the given association list *)

  val add_entry : state -> value -> ptr
  (** [add_entry st c] turns [c] into a heap entry and returns
      a pointer to it.
      The pointer is a small value value (a tagged integer). *)

  val add_entry_hashcons : state -> value -> ptr
  (** [add_entry_hashcons st c] turns [c] into a heap entry.
      [c] is first compared to existing
      hashconsed entries (at a runtime cost) to see if we can reuse
      them instead of inserting a new value. *)

  val add_string : ?hashcons:bool -> state -> string -> value
  (** Same as [add_entry state (`Text s)], except that large strings
      will be hashconsed unconditionally. *)

  val add_bytes : ?hashcons:bool -> state -> string -> value
  (** Same as {!add_string} *)

  val delay : (unit -> 'a t) -> 'a t
  (** [delay f] is like [f()], but [f] is only called when needed. *)

  val fix : ('a t -> 'a t) -> 'a t
  (** [fix f] is a recursive serializer. [f] receives a serializer
      for recursive cases and must use it to implement the serialization
      for the current value. *)

  val result : 'a t -> 'b t -> ('a, 'b) Stdlib.result t

  type 'a cache_key

  val create_cache_key :
    (module Hashtbl.HashedType with type t = 'a) -> 'a cache_key
  (** Create a new (generative) cache key for a hashable + comparable type.

      {b NOTE} this should be called only at module toplevel, as a constant,
      not dynamically inside a function:
      [let key = value_pack.Ser.create_cache_key (module …);;].
      Indeed, this is generative, so creating multiple keys for a type
      will result in sub-par or inexistant caching. *)

  val with_cache : 'a cache_key -> 'a t -> 'a t
  (** [with_cache key enc] is the same encoder as [enc], but
      with caching. When encoding a value [x:'a],
      the cache [key] is used to detect if [x] was already
      encoded to some entry, and uses a pointer to this entry
      instead of re-serializing [x]. *)

  val finalize_value : state -> key:value -> value
  (** Turn the state into a pack with given [key] as entrypoint. *)

  val finalize_cbor_string : state -> key:value -> string
  (** Same as {!finalize_value} but also turns the resulting packed value
      into a string. *)
end

val to_cbor_string : 'a Ser.t -> 'a -> string
(** [to_cbor_string ser x] seralizes [x] using [ser], and returns the
    pack containing the shared heap, and an entry point. *)

val to_value : 'a Ser.t -> 'a -> value
(** Same as {!to_string} but without the value encoding step. *)

val pp_diagnostic : value Fmt.printer

(** Deserialization *)
module Deser : sig
  type state
  (** Deserialization state, containing the heap of value
      values. *)

  type ptr

  type 'a t = state -> value -> 'a
  (** A deserializer takes a value value, and returns a value of type ['a] from it.
      @raise Error in case of error *)

  val deref : state -> ptr -> value
  (** Get an item via its pointer.
      @raise Invalid_argument if the pointer is invalid. *)

  val return : 'a -> 'a t

  val fail : string -> 'a
  (** Fail to decode. *)

  val failf : ('a, unit, string, 'b) format4 -> 'a
  (** Fail to decode with a formatted message. *)

  val to_unit : unit t
  val to_int : int t
  val to_int64 : int64 t
  val to_bool : bool t
  val to_float : float t
  val to_list : value list t
  val to_list_of : 'a t -> 'a list t
  val to_map : (string * value) list t
  val to_dict : value Str_map.t t
  val to_ptr : value -> ptr
  val map_entry : k:string -> state -> value -> value

  val to_any_tag : state -> value -> int * value
  (** Deserialize an arbitrary tag *)

  val to_tag : int -> state -> value -> value
  (** Expect a particular tag. *)

  val to_text : string t
  val to_bytes : string t

  val deref_if_ptr : state -> value -> value
  (** If the value is a pointer, dereference it (recursively) *)

  val ptr_of_int : int -> ptr
  (** Entry point *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  (** Map combinator *)

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  (** Bind combinator *)

  val delay : (unit -> 'a t) -> 'a t
  (** [delay f] is like [f()], but [f] is only called when needed. *)

  val parse : string -> state result

  val parse_exn : string -> state
  (** @raise Error if it fails *)

  type 'a cache_key
  (** Generative key used to cache values during decoding *)

  val create_cache_key : unit -> _ cache_key
  (** Generate a new (generative) cache key for a type.

      {b NOTE} this should be called only at module toplevel, as a constant,
      not dynamically inside a function:
      [let key: foo value_pack.Deser.cache_key = value_pack.Deser.create_cache_key ();;].
      Indeed, this is generative, so creating multiple keys for a type
      will result in sub-par or inexistant caching. *)

  val with_cache : 'a cache_key -> 'a t -> 'a t
  (** [with_cache key dec] is the same decoder as [dec] but
      it uses [key] to retrieve values directly from
      an internal table for entries/values that have already
      been decoded in the past. This means that a value that was
      encoded with a lot of sharing (e.g in a graph, or a large
      string using {!Ser.add_string}) will be decoded only once.
  *)

  val fix : ('a t -> 'a t) -> 'a t
  (** [fix f] is a recursive deserializer. [f] receives a deserializer
      for recursive cases and must use it to implement the deserialization
      for the current value. *)

  val result : 'a t -> 'b t -> ('a, 'b) Stdlib.result t

  val entry_key : state -> value
  (** Entrypoint for the pack, as used in {!Ser.finalize_value}
      or {!Ser.finalize_string} *)

  val show_diagnostic : state -> string
  (** Show the content of the deserialized value using the diagnostic notation *)

  val pp_diagnostic : Format.formatter -> state -> unit
  (** Show the content of the deserialized value using the diagnostic notation *)
end

val of_value_exn : 'a Deser.t -> value -> 'a
(** [of_value_exn deser value] deserializes an object using [deser]
    from the shared heap [value.h], starting at [value.key]. *)

val of_value : 'a Deser.t -> value -> 'a result
(** Deserialize a pack into a value of type ['a] *)

val of_cbor_string_exn : 'a Deser.t -> string -> 'a
(** Parse value and deserialize it *)

val of_cbor_string : 'a Deser.t -> string -> 'a result
