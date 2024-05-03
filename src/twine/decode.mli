open Types

type decoder

val create : Slice.t -> decoder

type 'a t = decoder -> offset -> 'a

val deref_rec : offset t
(** Given any value, follow pointers until a non-pointer value is reached,
    a return its address. *)

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
  ('st1 -> 'st2 -> ('st1 -> 'st2 -> int -> offset -> unit) -> unit) t
(** Access arguments of a constructor *)

val cstor_descr : cstor_descriptor t
