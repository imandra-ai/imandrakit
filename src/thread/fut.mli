(** Future *)

include module type of Moonpool.Fut
(** @inline *)

type backtrace = Printexc.raw_backtrace
type exn_with_bt = exn * Printexc.raw_backtrace

(** {2 Helpers} *)

val pp : 'a Fmt.printer -> 'a t Fmt.printer
val peek_exn : 'a t -> 'a option

val unwrap : ('a, exn_with_bt) result -> 'a
(** Unwrap result by raising in case of error *)

val raise_with_bt : exn -> backtrace -> 'a
val map_iter : f:('a -> 'b t) -> 'a Iter.t -> 'b Iter.t t
val map_l : f:('a -> 'b t) -> 'a list -> 'b list t
val protect : finally:(unit -> unit t) -> (unit -> 'a t) -> 'a t
val reify_error : 'a t -> 'a or_error t
val bind_reify_error : ?on:Executor.t -> f:('a or_error -> 'b t) -> 'a t -> 'b t

val wait_vec : (_ t, _) Vec.t -> unit t
(** Wait for all futures in the vector to be done *)

val map_vec_array : ('a t, _) Vec.t -> 'a array t

(** {2 Type erasure} *)

type any = Any : _ t -> any

val pp_any : any Fmt.printer
val any_is_resolved : any -> bool
val any_is_success : any -> bool
val any_is_failed : any -> bool
val any_raise_if_failed : any -> unit
