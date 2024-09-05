(** Run sub-processes.

     This gives more control than the equivalent {!Unix} APIs. *)

type state

type t = private {
  stdin: Utils.Output.t;
  stdout: Utils.Input.t;
  stderr: Utils.Input.t;
  pid: int;
  _st: state;
}
[@@deriving show]
(** A sub-process *)

val run : ?env:string array -> string -> string list -> t
(** Run subprocess with given command *)

val run_shell : ?env:string array -> string -> t
(** Run subprocess with given command *)

val res_code : t -> int Moonpool.Fut.t
val wait : t -> int
val kill : t -> unit
val signal : t -> int -> unit

val stopped : t -> bool
(** We know that we have stopped the process *)
