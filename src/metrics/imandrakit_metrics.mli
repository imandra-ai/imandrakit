(** Set of metrics *)

type kind =
  | Counter
  | Gauge
[@@deriving show]

type histogram_data = private {
  name: string;
  bucket_boundaries: float array;  (** sorted *)
  buckets: Float.Array.t;
      (** size: bucket_boundaries+1, as there's a underflow bucket *)
}
(** A histogram *)

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

module Histogram : sig
  type t
  (** A histogram with explicit buckets *)

  val create : string -> buckets:float array -> t
  (** [create_int name ~buckets] makes a histogram. Each item in [buckets] (which will be sorted)
is used as the lower bound of a bucket. An additional bucket is created for values below the lowest
    item in [buckets] *)

  val add_sample : t -> float -> unit
  (** Add a value in the histogram. *)
end

val add_on_refresh : (unit -> unit) -> unit
(** Register a function to be called every time metrics
    are about to be emitted *)

val add_gc_metrics : unit -> unit
(** Register some standard metrics for the GC *)

val iter_all :
  int:(kind -> string -> int -> unit) ->
  float:(kind -> string -> float -> unit) ->
  hist:(histogram_data -> unit) ->
  unit ->
  unit
(** Iterate on all metrics. This first calls every callback
   registered to {!add_on_refresh}. *)

val emit_trace : unit -> unit
(** Emit into [trace] as int and float counters *)

(* TODO: optionally add a HTTP GET handler using prometheus,
   if [Tiny_httpd.prometheus] is installed *)
