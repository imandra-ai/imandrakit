(** Run sub-processes.

 This gives more control than the equivalent {!Unix} APIs. *)

type state

type t = {
  stdin: out_channel;
  stdout: in_channel;
  stderr: in_channel;
  pid: int;
  _st: state;
}
[@@deriving show]
(** A sub-process *)

val run : ?env:string array -> string -> string list -> t
(** Run subprocess with given command *)

val run_shell : ?env:string array -> string -> t
(** Run subprocess with given command *)

val wait : t -> int

val kill : t -> unit

val signal : t -> int -> unit
