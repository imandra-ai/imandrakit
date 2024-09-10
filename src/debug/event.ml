(** Event type for subscribers.

    The {!Event.t} type corresponds closely to callbacks in
    Trace subscribers. Because events are just values,
    they can be serialized, forwarded to other processes, etc. *)

type span = int64 [@@deriving show, yojson, twine]

let _ : span -> Trace_core.span = Fun.id

type user_data = Trace_subscriber.user_data =
  | U_bool of bool
  | U_float of float
  | U_int of int
  | U_none
  | U_string of string
[@@deriving show { with_path = false }, yojson, twine]

type flavor = Trace_subscriber.flavor =
  | Sync
  | Async
[@@deriving show { with_path = false }, yojson, twine]

type t =
  | E_init of { time_ns: float }
      (** Called when the subscriber is initialized in a collector *)
  | E_shutdown of { time_ns: float }
      (** Called when the collector is shutdown *)
  | E_name_thread of {
      time_ns: float;
      tid: int;
      name: string;
    }  (** Current thread is being named *)
  | E_name_process of {
      time_ns: float;
      tid: int;
      name: string;
    }  (** Current process is being named *)
  | E_enter_span of {
      __FUNCTION__: string option;
      __FILE__: string;
      __LINE__: int;
      time_ns: float;
      tid: int;
      data: (string * user_data) list;
      name: string;
      span: span;
    }  (** Enter a regular (sync) span *)
  | E_exit_span of {
      time_ns: float;
      tid: int;
      span: span;
    }
      (** Exit a span. This and [on_enter_span] must follow strict stack discipline *)
  | E_add_data of {
      data: (string * user_data) list;
      span: span;
    }  (** Add data to a regular span (which must be active) *)
  | E_message of {
      time_ns: float;
      tid: int;
      span: span option;
      data: (string * user_data) list;
      msg: string;
    }  (** Emit a log message *)
  | E_counter of {
      time_ns: float;
      tid: int;
      data: (string * user_data) list;
      name: string;
      n: float;
    }  (** Emit the current value of a counter *)
  | E_enter_manual_span of {
      __FUNCTION__: string option;
      __FILE__: string;
      __LINE__: int;
      time_ns: float;
      tid: int;
      parent: span option;
      data: (string * user_data) list;
      name: string;
      flavor: flavor option;
      trace_id: int;
      span: span;
    }  (** Enter a manual (possibly async) span *)
  | E_exit_manual_span of {
      time_ns: float;
      tid: int;
      name: string;
      data: (string * user_data) list;
      flavor: flavor option;
      trace_id: int;
      span: span;
    }  (** Exit a manual span *)
[@@deriving show { with_path = false }, yojson, twine]
