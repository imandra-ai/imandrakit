(** Manage sub-processes. *)

type t
(** A sub-process *)

exception Killed
(** Exception indicating that a process did not run to completion. *)

val run : ?env:string array -> string -> string list -> t
(** Runs subprocess with the given command and arguments. *)

val await : t -> (int, exn) result
(** Awaits the exit of a process. *)

val kill : ?is_group:bool -> ?max_wait_s:float -> t -> unit
(** Kills a process. *)

val signal : t -> int -> unit
(** Sends a signal to the process. *)

val on_exit : t -> (t -> (int, exn) result -> unit) -> unit
(** Registers a callback to be run (in a new thread) when the process exits. *)

val pid : t -> int
(** The process identifier of the process. *)

val stdin : t -> out_channel
(** Standard Input of the process. *)

val stdout : t -> in_channel
(** Standard Output of the process. *)

val stderr : t -> in_channel
(** Standard Error Output of the process. *)

val start_time : t -> Ptime.t
(** The time the process was started. *)

val stop_time : t -> Ptime.t option
(** The time the result of the process arrived. *)

val execution_time : t -> Ptime.span option
(** Wall-clock execution time. *)
