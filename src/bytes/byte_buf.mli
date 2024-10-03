type t = {
  mutable bs: bytes;  (** Underlying bytes *)
  mutable len: int;  (** Length of the prefix of [bs] that is valid *)
  bs0: bytes;  (** Initial buffer *)
}

val create : ?cap:int -> unit -> t
(** Create a new buffer with given initial capacity. *)

val length : t -> int
(** Current length. *)

val is_empty : t -> bool
(** [is_empty b] is [length b=0] *)

val capacity : t -> int
(** Current capacity (size of the array returned by {!bytes}) *)

val clear : t -> unit
(** Clear the buffer, setting length to 0 *)

val reset : t -> unit
(** Clear buffer and reset it to its initial size.
    This ensures the buffer doesn't monotonically grow to
    its biggest size so far. *)

val ensure_cap : t -> int -> unit
(** [ensure_cap buf n] ensures that [capacity buf >= n]. *)

val ensure_free : t -> int -> unit
(** [ensure_free buf n] ensures that at least [n] bytes are free at the end *)

val shrink_to : t -> int -> unit
(** [shrink_to buf n] reduces [length buf] to at most [n].
    Does nothing if the length is already <= n. *)

val add_char : t -> char -> unit
(** Push a character at the end. *)

val to_slice : t -> Byte_slice.t
val append_bytes : t -> bytes -> unit
val append_subbytes : t -> bytes -> int -> int -> unit
val append_string : t -> string -> unit
val append_substring : t -> string -> int -> int -> unit
val append_buf : t -> Buffer.t -> unit
val append_iter : t -> char Iter.t -> unit
val append_seq : t -> char Seq.t -> unit
val get : t -> int -> char
val unsafe_get : t -> int -> char
val set : t -> int -> char -> unit
val unsafe_set : t -> int -> char -> unit
val copy : t -> t

val contents : t -> string
(** Copy the internal data to a string *)

val contents_bytes : t -> bytes
(** Copy the internal data to a byte buffer *)

val iter : (char -> unit) -> t -> unit
val fold_left : ('a -> char -> 'a) -> 'a -> t -> 'a
val of_iter : char Iter.t -> t
val of_seq : char Seq.t -> t
val to_iter : t -> char Iter.t
val to_seq : t -> char Seq.t
