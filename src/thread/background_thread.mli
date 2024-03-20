(** Background thread, for running tasks asynchronously in a sequential fashion.

    This is a layer on top of {!Task_runner1} that adds futures.
*)

type t = Executor.t

val pp : t Fmt.printer

val start :
  ?active:Switch.t ->
  ?on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
  name:string ->
  unit ->
  t

val with_ :
  ?active:Switch.t ->
  ?on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
  name:string ->
  unit ->
  (t -> 'a) ->
  'a

val join : t -> unit
