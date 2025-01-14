open Types
module Immediate = Immediate

type immediate = Immediate.t
type t

val create : ?cap:int -> unit -> t
(** New encoder.
    @param cap initialize capacity of the underlying byte buffer *)

val clear : t -> unit
(** Clear the encoder, to reuse it.
    Previous slices obtained via {!finalize} are invalidated. *)

val reset : t -> unit
(** Fully reset the encoder. Previous slices obtained via {!finalize}
    are invalidated. *)

type 'a encoder = t -> 'a -> immediate

val ignore_offset : offset -> unit
(** Used when we don't use the resulting offset *)

(** {2 Primitives} *)

val write_immediate : t -> immediate -> offset
(** [write_immediate enc i] writes down [i] and returns
    an immediate pointer to it *)

val write_or_ref_immediate : t -> immediate -> offset
(** [write_or_deref_immediate enc i] looks at [i],
    and if [i] is [Pointer p], it returns [p]; otherwise
    it writes down [i] and returns a pointer to it. *)

val tag : t -> tag:int -> v:immediate -> immediate

(** {2 Arrays} *)

val array_init : t -> int -> (int -> immediate) -> immediate
val array : t -> immediate array -> immediate
val array_iter : t -> immediate Iter.t -> immediate
val list : t -> immediate list -> immediate

(** {2 Dictionaries} *)

val dict : t -> int -> (int -> immediate * immediate) -> immediate
val dict_iter : t -> (immediate * immediate) Iter.t -> immediate
val dict_list : t -> (immediate * immediate) list -> immediate

(** {2 Sum types} *)

val cstor : t -> index:int -> immediate array -> immediate

(** {2 Top value} *)

val finalize : t -> entrypoint:immediate -> slice
(** Finalize by writing the entrypoint, and get the result as a byte slice
 @param top the offset of the toplevel/entrypoint  value *)

val finalize_copy : t -> entrypoint:immediate -> string
(** Finalize, and get a copy of the result. See {!finalize} *)

val encode_to_string : ?encoder:t -> 'a encoder -> 'a -> string
(** Full entrypoint *)

val to_string : ?encoder:t -> 'a encoder -> 'a -> string
[@@deprecated "use encode_to_string instead"]

(** {2 Caching}

    Caching is used to create/preserve sharing in the encoded slice,
    by actively storing, in a hashtable, the offset where a value
    was previously encoded. *)

type 'a cache_key

val create_cache_key :
  (module Hashtbl.HashedType with type t = 'a) -> 'a cache_key
(** Create a new (generative) cache key for a hashable + comparable type.

    {b NOTE} this should be called only at module toplevel, as a constant,
    not dynamically inside a function:
    [let key = create_cache_key (module …);;].
    Indeed, this is generative, so creating multiple keys for a type
    will result in sub-par or inexistant caching. *)

val with_cache :
  ?max_string_size:int ->
  ?skip:('a -> bool) ->
  'a cache_key ->
  'a encoder ->
  'a encoder
(** [with_cache key enc] is the same encoder as [enc], but
    with caching. When encoding a value [x:'a],
    the cache [key] is used to detect if [x] was already
    encoded to some entry, and uses a pointer to this entry
    instead of re-serializing [x].
    @param max_string_size strings and blobs above this size
    are written once and referred to by pointer
*)

val add_cache :
  ?max_string_size:int ->
  ?skip:('a -> bool) ->
  (module Hashtbl.HashedType with type t = 'a) ->
  'a encoder ref ->
  unit
(** [add_cache (module …) enc_ref] modifies the given encoder so that
    it goes through a layer of caching. This is the same
    as:
      {[
let key = create_cache_key (module …)
let () = enc_ref := with_cache key !enc_ref
      ]}
    @param skip if true, do not attempt to cache this value. *)

val add_cache_with :
  ?max_string_size:int ->
  ?skip:('a -> bool) ->
  eq:('a -> 'a -> bool) ->
  hash:('a -> int) ->
  'a encoder ref ->
  unit
(** Shortcut for {!add_cache} *)
