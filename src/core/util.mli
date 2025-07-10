(** Utils *)

val ptime_now : unit -> float
(** Use [ptime] to get a precise timestamp. *)

val time_since_start : unit -> float
(** Time since the beginning of the program, using {!mtime_now} *)

val mtime_now_s : unit -> float
(** Alias for {!time_since_start} *)

(** Stopwatch, to measure time intervals. *)
module Stopwatch : sig
  type t

  val create : unit -> t
  (** New stopwatch. *)

  val time : t -> float
  (** Time elapsed since creation. If the stopwatch was stopped using {!stop}
      then this returns the span of time between creation and call to {!stop}.
  *)

  val stop : t -> unit
  (** Stop the watch. It will always return the same time from now on. *)

  val timeit : (unit -> 'a) -> float * 'a
  (** [timeit f] returns how long [f()] took to run *)
end

val remove_dups_with :
  (module CCHashtbl.S with type key = 'a) -> 'a list -> 'a list

val pp_text_newlines : string Fmt.printer
val pp_list : ?sep:string -> 'a Fmt.printer -> 'a list Fmt.printer
val pp_iter : ?sep:string -> 'a Fmt.printer -> 'a Iter.t Fmt.printer

val pp_backquote : 'a Fmt.printer -> 'a Fmt.printer
(** print between "`" *)

val pp_quoted : string Fmt.printer
val pp_atomic : 'a Fmt.printer -> 'a Atomic.t Fmt.printer

val str_contains : string -> string -> bool
(** String containment. [s2] must be small *)

val str_limit_len : int -> string -> string
(** Limit length of string *)

val parse_tcp_addr : string -> (Unix.inet_addr * int, string) result

val uuid_v4 : unit -> string
(** Generate a UUID v4 using a random state *)

val uuid_v7_ptime : unit -> Uuidm.t
(** Generate a UUID v7 using a random state and [Ptime_clock.now()] *)

val uuid_v7_ptime_str : unit -> string

val this_process_uuid : string
(** UUID for this process. Computed once at startup. *)

val true_in_env : string -> bool
(** [true_in_env s] is [true] if ["s"] is set to "1" or "true" in the env *)

val format_datetime : float -> string
(** Helper to format time nicely *)

val pp_datetime : float Fmt.printer

val format_duration_s : float -> string
(** Human friendly duration printer *)

val pp_duration_s : float Fmt.printer

val format_byte_size : int -> string
(** Human friendly size (in Bytes) printer *)

val pp_byte_size : int Fmt.printer
val reset_line_ansi : string

module List : sig
  val split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
end

module Either = CCEither

type ('a, 'b) either = ('a, 'b) CCEither.t =
  | Left of 'a
  | Right of 'b

val pp_either : 'a Fmt.printer -> 'b Fmt.printer -> ('a, 'b) either Fmt.printer
val min_opt_int : int option -> int option -> int option
