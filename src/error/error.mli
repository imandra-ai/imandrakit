(** Universal error type for Imandrakit.

    Exposes the standard errors that Imandrakit can raise. *)

include module type of Error_core
module Kind = Kind

val fail :
  ?stack:message list ->
  ?process:string ->
  ?bt:string ->
  kind:Kind.t ->
  string ->
  'a
(** [fail "some error message"]
    raises an error with the given message *)

val failf :
  ?stack:message list ->
  ?process:string ->
  ?bt:string ->
  kind:Kind.t ->
  ('a, Format.formatter, unit, 'b) format4 ->
  'a
(** [errorf "some error message %s (number %d)" "with formatting" 42]
    raises an error with the given formatted message *)

val mk_error :
  ?stack:message list ->
  ?process:string ->
  ?bt:string ->
  kind:Kind.t ->
  string ->
  t

val mk_errorf :
  ?stack:message list ->
  ?process:string ->
  ?bt:string ->
  kind:Kind.t ->
  ('a, Format.formatter, unit, t) format4 ->
  'a

val message : ?bt:string -> string -> message
val messagef : ?bt:string -> ('a, Format.formatter, unit, message) format4 -> 'a
val guards : ?let_pass:(exn -> bool) -> string -> (unit -> 'a) -> 'a

val guardf :
  ?let_pass:(exn -> bool) ->
  ((('a, Format.formatter, unit, message) format4 -> 'a) -> message) ->
  (unit -> 'b) ->
  'b
(** [guardf ~loc k f] behaves like [f()], but will call [k] to produce a
    contextual message in case of error. *)

val of_exn : ?bt:Printexc.raw_backtrace -> kind:Kind.t -> exn -> t
(** Turn exception into an error. *)

val of_exn_bt : kind:Kind.t -> Moonpool.Exn_bt.t -> t
(** Turn exception into an error. *)

val try_catch : kind:Kind.t -> unit -> (unit -> 'a) -> 'a result
val unwrap_opt : 'a option -> 'a
