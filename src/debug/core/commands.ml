open Types

(** Request, from debug frontend *)
module Request = struct
  type t =
    | R_enable of { level: level }
    | R_disable
    | R_set_level of level
    | R_list_threads
    | R_get_proc_name
    | R_get_thread of { tid: int }
    | R_get_all_threads
    | R_get_counter of { name: string }
    | R_get_all_counters
  [@@deriving show { with_path = false }, yojson, twine]
end

module Response = struct
  type time_ns = float [@@deriving show, yojson, twine]

  type thread_info = {
    tid: tid;
    name: string option;
  }
  [@@deriving show { with_path = false }, yojson, twine]

  type thread_state = {
    t: thread_info;
    stack: span_info array;
  }
  [@@deriving show { with_path = false }, yojson, twine]

  type counter_state = {
    name: string;
    time_ns: time_ns;
    v: float;
  }
  [@@deriving show { with_path = false }, yojson, twine]

  type t =
    | R_ok
    | R_error of string
    | R_proc_name of {
        pid: int;
        name: string option;
      }
    | R_list_threads of thread_info list
    | R_thread of thread_state
    | R_all_threads of thread_state list
    | R_counter of counter_state
    | R_all_counters of counter_state list
  [@@deriving show { with_path = false }, yojson, twine]
end

module Server_notification = struct
  type t =
    | N_enabled of { level: level }
    | N_disabled
    | N_events of Event.t list
    | N_disconnect  (** Client gets disconnected *)
  [@@deriving show { with_path = false }, yojson, twine]
end
