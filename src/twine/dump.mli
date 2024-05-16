(** Produce a debug dump of the given Twine value *)

open Types

val dump_slice : ?string_ellipsis_threshold:int -> slice -> string
(** Produce a debug dump of the twine blob, displaying individual values
  at their offset.
  @param string_ellipsis_threshold limit above which strings start to be
  truncated when they're dumped *)

val dump_string : ?string_ellipsis_threshold:int -> string -> string
(** Like {!dump_slice} *)
