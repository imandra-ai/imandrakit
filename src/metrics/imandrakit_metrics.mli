(** Set of metrics *)

type kind =
  | Counter
  | Gauge
[@@deriving show]

(** A counter, increasing monotonically *)
module Counter : sig
  type 'a t

  val create_int : string -> int t
  val create_float : string -> float t
  val set : 'a t -> 'a -> unit
  val incr : int t -> unit

  val incr_by : int t -> int -> unit
  (** [incr_by c n] adds [n] to counter [c].
      @raise Assert_failure if [n < 0] *)
end

(** A gauge, representing some value fluctuating over time *)
module Gauge : sig
  type 'a t

  val create_int : string -> int t
  val create_float : string -> float t
  val set : 'a t -> 'a -> unit
end

val add_on_refresh : (unit -> unit) -> unit
(** Register a function to be called every time metrics
    are about to be emitted *)

val add_gc_metrics : unit -> unit
(** Register some standard metrics for the GC *)

val iter_all :
  int:(kind -> string -> int -> unit) ->
  float:(kind -> string -> float -> unit) ->
  unit ->
  unit
(** Iterate on all metrics. This first calls every callback
   registered to {!add_on_refresh}. *)

val emit_trace : unit -> unit
(** Emit into [trace] as int and float counters *)

val emit_trace_every : period_s:float -> Timer.t -> unit
(** Emit metrics every [period_s] seconds *)

(* TODO: optionally add a HTTP GET handler using prometheus,
   if [Tiny_httpd.prometheus] is installed *)
