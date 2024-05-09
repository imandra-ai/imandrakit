open Types

type t

val create : slice -> t
val of_string : string -> t

type 'a decoder = t -> offset -> 'a
type cstor_index = int

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
    | Pointer of offset
    | Array of array_cursor
    | Dict of dict_cursor
    | Tag of int * offset
    | Cstor0 of cstor_index
    | Cstor1 of cstor_index * offset
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
  val to_list : t -> Value.t list
end

module Dict_cursor : sig
  type t = Value.dict_cursor [@@deriving show]

  val length : t -> int
  val current : t -> offset * offset
  val consume : t -> unit
  val get_key_value_and_consume : t -> Value.t * Value.t
  val to_iter : t -> (Value.t * Value.t) Iter.t
  val to_list : t -> (Value.t * Value.t) list
end

val deref_rec : offset decoder
(** Given any value, follow pointers until a non-pointer value is reached,
    a return its address. *)

val read : Value.t decoder
(** Read a value of any kind *)

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
val dict : Dict_cursor.t decoder
val tag : (int * offset) decoder
val cstor : (cstor_index * Array_cursor.t) decoder

(** {2 Entrypoint} *)

val get_entrypoint : t -> offset
(** Offset of the entrypoint (the topevel value) *)

val read_entrypoint : t -> Value.t
(** Read the entrypoint, from the end of the slice *)

val decode_string : 'a decoder -> string -> 'a

(*

type 'a t = decoder -> offset -> 'a

val tag : tag t
(** Access tag at this offset. *)

val int64 : int64 t
(** Assuming tag is int, read the integer *)

val int_truncate : int t
(** Like {!int} but truncates from int64 to int *)

val float : float t
(** Assuming tag is float, read the float *)

val string : string t
val string_ref : (string * int * int) t
val blob : string t
val blob_ref : (string * int * int) t

val pointer : offset t
(** Dereference a pointer *)

val array : ('st1 -> 'st2 -> ('st1 -> 'st2 -> int -> offset -> unit) -> unit) t
(** Access items in an array. *)

val dict :
  ('st1 -> 'st2 -> ('st1 -> 'st2 -> string -> offset -> unit) -> unit) t
(** Access content of a dictionary *)

val key_content : offset t
(** After reading a key, access the value *)

val record_descr : record_descriptor t
(** Access a record descriptor *)

val record_fields :
  ('st1 ->
  'st2 ->
  ('st1 -> 'st2 -> index:int -> name:string -> offset -> unit) ->
  unit)
  t
(** Access the fields of a record.  This will also
    read the record descriptor implicitly. *)

val cstor_num : int t
(** Index of a given constructor. *)

val cstor_args :
 the grand promises, and no actual the grand promises, and no actual  ('st1 -> 'st2 -> ('st1 -> 'st2 -> int -> offset -> unit) -> unit) t
(** Access arguments of a constructor *)

val cstor_descr : cstor_descriptor t
*)
