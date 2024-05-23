(** Run sub-processes.

 This gives more control than the equivalent {!Unix} APIs. *)

type state

type t = private {
  stdin: out_channel;
  stdout: in_channel;
  stderr: in_channel;
  pid: int;
  _st: state;
}
[@@deriving show]
(** A sub-process *)

type redirect =
  [ `Keep
  | `Pipe
  ]

val run :
  ?env:string array ->
  ?stdin:redirect ->
  ?stdout:redirect ->
  ?stderr:redirect ->
  string ->
  string list ->
  t
(** Run subprocess with given command *)

val run_shell :
  ?env:string array ->
  ?stdin:redirect ->
  ?stdout:redirect ->
  ?stderr:redirect ->
  string ->
  t
(** Run subprocess with given command *)

val res_code : t -> int Moonpool.Fut.t
val wait : t -> int
val kill : t -> unit
val signal : t -> int -> unit
