(** Main logger *)

type level = Log_level.t [@@deriving show, eq]

module Log_event = Log_event

(** Log output.

    We can have multiple log outputs, they each get a {!Log_event.t}.
    See {!Reporter.to_outputs} to see how to use these.
*)
module Output : sig
  type t
  (** A log output. It receives {!Log_event.t}s and
      writes/sends them somewhere. *)

  val stdout : unit -> t

  val stderr : unit -> t

  val to_event : emit_ev:(Log_event.t -> unit) -> unit -> t

  val filter_level : (level -> bool) -> t -> t

  val to_str : emit_str:(string -> unit) -> unit -> t

  val to_chan : out_channel -> t
  (** Write into the channel, as text. *)

  val to_chan_jsonl : out_channel -> t
  (** Write into the channel as jsonl. *)
end

type t
(** Main logger. This obtains events from {!Logs} and
    writes them to the current set of {!Output.t}. *)

val shutdown : t -> unit

val as_reporter : t -> Logs.reporter

val events : t -> Log_event.t Observer.t

val null : unit -> t

val add_output : t -> Output.t -> unit

val to_outputs : Output.t list -> t
(** Sends events to each output in the list *)

val with_no_logger : unit -> (unit -> 'a) -> 'a
(** [with_no_logger () f] calls [f()] in a context where there
  is no logger. Useful to avoid logging loops. *)

val setup_level : ?debug:bool -> ?log_level:level -> unit -> unit
(** Setup log level. It will use [Info]  by default, unless
    the env var ["DEBUG"] is set or [~debug] or [~log_level] is passed.

    [debug] takes precedence over [log_level] which takes precedence over the env. *)

val setup_logger_to_stdout : unit -> unit
(** Setup a logger that emits on stdout *)

val setup_logger_to_stderr : unit -> unit
(** Setup a logger that emits on stderr *)

val setup_logger_to_LOG_FILE : ?filename:string -> unit -> (unit -> 'a) -> 'a
(** Setup a logger that emits into the file specified in [filename]
    or in ["LOG_FILE"] env, or no logger otherwise.
    This will cleanup at the end when the function returns. *)

module type LOG = sig
  include Logs.LOG

  val src : Logs.src
end

val fence : unit -> unit
(** [fence ()] waits for the logger to have written all already
    queued messages. *)

val mk_log_str : string -> (module LOG)
(** Create a new logger with the given source. *)

val all_sources : Logs.src Iter.t
(** Get all log sources *)

val setup_level_per_source : (string * level option) Iter.t -> unit
(** Set level for certain sources *)
