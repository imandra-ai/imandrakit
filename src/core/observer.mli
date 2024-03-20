(** Observer, to subscribe to some events.

    The observer is thread-safe. However, it is recommended
    that [subscribe] callbacks either run in a very short time,
    or queue heavier work on some background {!Executor_sig.S}. *)

type 'a t
(** Observer of events of type ['a] *)

val create : unit -> 'a t

val emit : 'a t -> 'a -> unit

type handle

exception Unsubscribe

val subscribe : 'a t -> ('a -> unit) -> handle
(** Subscribe to the events.
    The callback can raise [Unsubscribe] to remove itself. *)

val subscribe' : 'a t -> ('a -> unit) -> unit
(** Subscribe to the events.
    The callback can raise [Unsubscribe] to remove itself. *)

val unsubscribe : _ t -> handle -> unit

val n_subscribers : _ t -> int
(** Current number of subscribers *)

val has_subscribers : _ t -> bool
(** Is there any subscriber? *)

val forward : 'a t -> 'a t -> unit
(** [forward o1 o2] subscribes to [o1] and forwards all events
    to [o2]. *)

val with_subscribe : 'a t -> ('a -> unit) -> (unit -> 'ret) -> 'ret
(** [with_subscribe obs cb f] subscribes [cb] to [obs],
    calls [f()], and when [f()] is done or has raised,
    it unsubscribes [cb]. *)
