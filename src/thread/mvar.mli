(** MVar.

  A mvar is a thread-safe box containing 0 or 1 element. Trying to take from
  an empty box, or trying to put into a full box, will both block.

  See for example {{: https://hackage.haskell.org/package/base-4.16.2.0/docs/Control-Concurrent-MVar.html} haskell}
*)

type 'a t

val create_empty : unit -> 'a t

val create_full : 'a -> 'a t

val clear : _ t -> unit
(** Remove content, if any *)

val take_block : 'a t -> 'a

val try_take : 'a t -> 'a option

val put_block : 'a t -> 'a -> unit

val peek : 'a t -> 'a option
(** Access current content without removing it. Only use with immutable content. *)

val try_put : 'a t -> 'a -> bool
(** [try_put mv x] tries to put [x] in [m]. It returns [true] on success,
    [false] if the attempt failed *)

val update_block : 'a t -> ('a -> 'a * 'b) -> 'b
(** [update_block mv f] takes [x] from [mv], calls [f], and puts the new
    state back into [mv]. It returns a secondary value.
    [f] is called in a critical section and should not block or take
    other locks; if it does, beware of deadlocks! *)

val pp : 'a Fmt.printer -> 'a t Fmt.printer
