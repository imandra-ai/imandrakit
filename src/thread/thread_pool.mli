(** Thread Pool.

 This is a wrapper around moonpool's thread pools. *)

type t = Executor.t

val start :
  ?active:Switch.t ->
  ?on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
  ?kind:[ `Fifo | `WorkStealing ] ->
  name:string ->
  j:int ->
  unit ->
  t
(** Start a new pool with [j] job runners.
    @param on_exn called when a task fails.
    @param j number of thread workers. Must be positive.
    @param kind the underlying pool implementation. See Moonpool
    for more details.
*)

val with_ :
  ?active:Switch.t ->
  ?on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
  ?kind:[ `Fifo | `WorkStealing ] ->
  name:string ->
  j:int ->
  unit ->
  (t -> 'a) ->
  'a

val default_size : unit -> int
(** Recommended size for a thread pool *)
