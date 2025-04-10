(** Lock with fast read path for immutable data *)

type 'a t
(** A lock for immutable data. Thread safe.

    Reading is really fast (one atomic read), writing is serialized using a
    mutex. *)

val create : 'a -> 'a t

val get : 'a t -> 'a
(** Read *)

val update : 'a t -> ('a -> 'a) -> unit
(** [update l f] replaces the content [x] of [l] with [f x], atomically. *)

val update_map : 'a t -> ('a -> 'a * 'b) -> 'b
(** [update_map l f] computes [x', y = f (get l)], then puts [x'] in [l] and
    returns [y]. *)

val pp : 'a Fmt.printer -> 'a t Fmt.printer
