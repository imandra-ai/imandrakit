(** Current state of a set of traced processes *)

type t
(** State tracking a lot of current state for potentially multiple processes *)

val create : unit -> t

val global_st : t
(** State used for debugging the current process *)

val set_on_block : t -> (tid -> unit) -> unit

val subscriber : t -> Trace_subscriber.t
(** As a subscriber *)

val suspend_thread : t -> tid:int -> bool

(** {2 Client interface} *)

module Client_state : sig
  type t
  (** State for a single client *)

  val is_active : t -> bool
  val enable_notifications : t -> unit
  val disable_notifications : t -> unit
  val set_level : t -> level -> unit
  val id : t -> int
end

val add_client :
  t -> on_notif:(Commands.Server_notification.t -> unit) -> Client_state.t

val remove_client : t -> Client_state.t -> unit
val num_clients : t -> int

val handle_req :
  t -> Client_state.t -> Commands.Request.t -> Commands.Response.t

(**/**)

module Private_ : sig
  val block_current_thread : unit -> unit
  val continue : t -> unit
  val is_blocked : unit -> bool
end

(* DAP friendly types below *)

type thread_info = {
  id: int;
  spans: string list;
  name: string;
}

val threads : t -> thread_info list

type source_loc = {
  name: string;
  path: string;
  line: int;
  column: int;
}

type stack_frame = {
  id: int;
  name: string;
  source: source_loc;
}

val stack_frames :
  t -> tid -> int option -> int option -> stack_frame list * int

type scope = {
  name: string;
  variables_reference: int;
}

val scopes : t -> int -> scope list

type variable = {
  name: string;
  value: string;
  type_: string;
  variables_reference: int;
}

val variables : t -> int -> variable list
val evaluate : t -> int option -> string -> (string * int) option

(**/**)
