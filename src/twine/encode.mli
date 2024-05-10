open Types
module Immediate = Immediate

type immediate = Immediate.t
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

val with_cache : 'a cache_key -> 'a encoder -> 'a encoder
(** [with_cache key enc] is the same encoder as [enc], but
    with caching. When encoding a value [x:'a],
    the cache [key] is used to detect if [x] was already
    encoded to some entry, and uses a pointer to this entry
    instead of re-serializing [x].
*)
