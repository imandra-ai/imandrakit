(** Current state of a set of traced processes *)

type t
(** State tracking a lot of current state for potentially multiple processes *)

val create : unit -> t

val global_st : t
(** State used for debugging the current process *)

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
end

val add_client :
  t -> on_notif:(Commands.Server_notification.t -> unit) -> Client_state.t

val remove_client : t -> Client_state.t -> unit

val handle_req :
  t -> Client_state.t -> Commands.Request.t -> Commands.Response.t

(**/**)

module Private_ : sig
  val block_current_thread : unit -> unit
end

(**/**)
