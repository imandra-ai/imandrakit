open Common_

(* module Pid_tbl = Int_tbl *)
module Tid_tbl = Sharded_tbl.Make8 (CCInt)
module Span_tbl = Sharded_tbl.Make16 (Int64)
module Counter_tbl = Sharded_tbl.Make16 (CCString)
module Vec = CCVector

type span_info = {
  span: span;
  name: string;
  __FUNCTION__: string option;
  __FILE__: string;
  __LINE__: int;
  time_ns: float;
  mutable data: (string * user_data) list;
      (* TODO: list of [string * (Var : 'a * 'a poke_tc)], to examine variables
         dynamically *)
}
(** A regular synchronous span *)

type thread_state = {
  tid: tid;
  spans: span_info Vec.vector;
  mutable name: string option;
}
(** State for a given thread *)

(* TODO:
   type async_span_info = {
     span: span;
     name: string;
     __FUNCTION__: string option;
     __FILE__: string;
     __LINE__: int;
     time_ns: float;
     data: (string * user_data) list;
     trace_id: int;
     flavor: flavor option;
   }
*)

type counter_info = {
  name: string;
  mutable time_ns: float;
  mutable v: float;  (** Last value *)
}

(* TODO: type async_trace = { spans: async_span_info Span_tbl.t } [@@unboxed] *)

type process_state = {
  pid: int;
  mutable name: string option;
  threads: thread_state Tid_tbl.t;
      (** Threads for current process (if any) *)
      (* async: async_trace Str_tbl.t Lock.t; (* TODO: tid -> breakpoint ? *) *)
}

type t = {
  client_id_gen: int Atomic.t;
  cur_proc: process_state;  (** Current process (if any) *)
  (* other_proc: process_state Pid_tbl.t Lock.t;  (** Other processes (if any) *) *)
  n_clients_with_notifs: int Atomic.t;
      (** How many clients have notifications enabled *)
  spans: span_info Span_tbl.t;  (** All active spans *)
  counters: counter_info Counter_tbl.t;  (** All counters *)
  clients: client_state Int_map.t Immlock.t;  (** Current clients *)
  mutable emit_ev_st: Subscribers.To_events_callbacks.st option;
}

and client_state = {
  inside_of: t;
  id: int;  (** Unique handle for this client state *)
  active: bool Atomic.t;
  wants_notifications: bool Atomic.t;
  mutable level: Trace.Level.t;
  on_notif: Commands.Server_notification.t -> unit;
}

let create_proc_state ~pid () : process_state =
  {
    pid;
    name = None;
    threads = Tid_tbl.create ();
    (* async = Lock.create @@ Str_tbl.create 16; *)
  }

let broadcast_ev_ (self : t) ev : unit =
  let clients = Immlock.get self.clients in
  Int_map.iter
    (fun _ cl ->
      if Atomic.get cl.wants_notifications then
        cl.on_notif @@ Commands.Server_notification.N_events [ ev ])
    clients

let create () : t =
  let st =
    {
      client_id_gen = Atomic.make 0;
      cur_proc = create_proc_state ~pid:(Unix.getpid ()) ();
      spans = Span_tbl.create ();
      counters = Counter_tbl.create ();
      (* other_proc = Lock.create @@ Pid_tbl.create 16; *)
      n_clients_with_notifs = Atomic.make 0;
      clients = Immlock.create Int_map.empty;
      emit_ev_st = None;
    }
  in
  st.emit_ev_st <-
    Some (Subscribers.To_events_callbacks.St { st; emit_ev = broadcast_ev_ });
  st

let with_add_or_create_thread_state_ (self : process_state) ~tid
    (f : thread_state -> 'a) : 'a =
  Tid_tbl.with_add_or_create self.threads tid
    ~create:(fun tid -> { tid; name = None; spans = Vec.create () })
    f

let with_find_thread_state_ (self : process_state) ~tid
    (f : thread_state option -> 'a) : 'a =
  Tid_tbl.with_find self.threads tid f

module Client_state = struct
  type t = client_state

  let enable_notifications self =
    if not (Atomic.exchange self.wants_notifications true) then
      Atomic.incr self.inside_of.n_clients_with_notifs

  let disable_notifications self =
    if Atomic.exchange self.wants_notifications false then
      Atomic.decr self.inside_of.n_clients_with_notifs

  let set_level self l = self.level <- l
end

module Callbacks : Trace_subscriber.Callbacks.S with type st = t = struct
  module EV_CB = Subscribers.To_events_callbacks

  type st = t

  let[@inline] _st (self : t) : EV_CB.st =
    match self.emit_ev_st with
    | Some s -> s
    | None -> assert false

  let on_init self ~time_ns =
    if Atomic.get self.n_clients_with_notifs > 0 then
      EV_CB.on_init (_st self) ~time_ns

  let on_shutdown self ~time_ns =
    if Atomic.get self.n_clients_with_notifs > 0 then
      EV_CB.on_shutdown (_st self) ~time_ns

  let on_name_thread self ~time_ns ~tid ~name =
    (let@ st = with_add_or_create_thread_state_ self.cur_proc ~tid in
     st.name <- Some name);

    if Atomic.get self.n_clients_with_notifs > 0 then
      EV_CB.on_name_thread (_st self) ~time_ns ~tid ~name

  let on_name_process self ~time_ns ~tid ~name =
    self.cur_proc.name <- Some name;
    if Atomic.get self.n_clients_with_notifs > 0 then
      EV_CB.on_name_process (_st self) ~time_ns ~tid ~name

  let on_enter_span self ~__FUNCTION__ ~__FILE__ ~__LINE__ ~time_ns ~tid ~data
      ~name span =
    (let@ tst = with_add_or_create_thread_state_ self.cur_proc ~tid in
     Vec.push tst.spans
       ({ __FUNCTION__; __FILE__; __LINE__; time_ns; name; data; span }
         : span_info));
    if Atomic.get self.n_clients_with_notifs > 0 then
      EV_CB.on_enter_span (_st self) ~__FUNCTION__ ~__FILE__ ~__LINE__ ~time_ns
        ~tid ~data ~name span

  let on_exit_span self ~time_ns ~tid span =
    (* update our local state *)
    Span_tbl.remove self.spans span;
    (with_find_thread_state_ self.cur_proc ~tid @@ function
     | None ->
       Log.err (fun k ->
           k "exiting span %a in unknown thread %d" pp_span span tid)
     | Some t ->
       (match Vec.top_exn t.spans with
       | exception Vec.Empty ->
         Log.err (fun k ->
             k "exiting span %a in thread %d with no spans" pp_span span tid)
       | sp when sp.span <> span ->
         Log.err (fun k ->
             k "exiting span %a in thread %d whose top span is %a" pp_span span
               tid pp_span sp.span)
       | _sp -> ignore (Vec.pop_exn t.spans : span_info)));

    if Atomic.get self.n_clients_with_notifs > 0 then
      EV_CB.on_exit_span (_st self) ~time_ns ~tid span

  let on_add_data self ~data span =
    (match Span_tbl.find_exn self.spans span with
    | sp -> sp.data <- List.rev_append data sp.data
    | exception Not_found ->
      Log.err (fun k -> k "span %a is not active" pp_span span));
    if Atomic.get self.n_clients_with_notifs > 0 then
      EV_CB.on_add_data (_st self) ~data span

  let on_message self ~time_ns ~tid ~span ~data msg =
    if Atomic.get self.n_clients_with_notifs > 0 then
      EV_CB.on_message (_st self) ~time_ns ~tid ~span ~data msg

  let on_counter self ~time_ns ~tid ~data ~name v =
    (let@ c =
       Counter_tbl.with_add_or_create self.counters name ~create:(fun name ->
           { name; time_ns = 0.; v = 0. })
     in
     c.time_ns <- time_ns;
     c.v <- v);

    if Atomic.get self.n_clients_with_notifs > 0 then
      EV_CB.on_counter (_st self) ~time_ns ~tid ~data ~name v

  (* TODO: *)
  [@@@ocaml.warning "-27"]

  let on_enter_manual_span self ~__FUNCTION__ ~__FILE__ ~__LINE__ ~time_ns ~tid
      ~parent ~data ~name ~flavor ~trace_id span =
    (* TODO: update async spans *)
    ()

  let on_exit_manual_span self ~time_ns ~tid ~name ~data ~flavor ~trace_id span
      =
    (* TODO: update async spans *)
    ()

  [@@@ocaml.warning "+27"]
end

let callbacks : t Trace_subscriber.Callbacks.t = (module Callbacks)

let subscriber (self : t) =
  Trace_subscriber.Subscriber.Sub { st = self; callbacks }

let add_client (self : t) ~on_notif : client_state =
  let cl =
    {
      inside_of = self;
      id = Atomic.fetch_and_add self.client_id_gen 1;
      active = Atomic.make true;
      level = Debug1;
      wants_notifications = Atomic.make false;
      on_notif;
    }
  in
  Immlock.update self.clients (Int_map.add cl.id cl);
  cl

let remove_client (self : t) cl : unit =
  if Atomic.exchange cl.active false then
    cl.on_notif @@ Commands.Server_notification.N_disconnect;
  Immlock.update self.clients (Int_map.remove cl.id)

let handle_req (self : t) (cl : client_state) (req : Commands.Request.t) :
    Commands.Response.t =
  let module Res = Commands.Response in
  let get_t_state (t : thread_state) : Res.thread_state =
    let stack =
      t.spans
      |> Vec.map
           (fun
             ({
                name;
                __FUNCTION__;
                __FILE__;
                __LINE__;
                time_ns;
                data;
                span = _;
              } :
               span_info)
             :
             Imandrakit_debug_core.span_info
           -> { name; __FUNCTION__; __FILE__; __LINE__; time_ns; data })
      |> Vec.to_array
    in
    { Res.t = { name = t.name; tid = t.tid }; stack }
  in

  let get_counter (c : counter_info) : Res.counter_state =
    let { name; time_ns; v } = c in
    { name; time_ns; v }
  in

  match req with
  | R_enable _ -> Res.R_ok (* TODO: install subscriber? *)
  | R_disable ->
    remove_client self cl;
    Res.R_ok
  | R_set_level lvl ->
    if lvl < cl.level && lvl < Trace.get_current_level () then
      Trace.set_current_level lvl;
    cl.level <- lvl;
    (* might need to update level *)
    Res.R_ok
  | R_get_proc_name ->
    Res.R_proc_name { name = self.cur_proc.name; pid = self.cur_proc.pid }
  | R_list_threads ->
    let l = ref [] in
    Tid_tbl.iter self.cur_proc.threads (fun _ (t : thread_state) ->
        l := { Res.name = t.name; tid = t.tid } :: !l);
    Res.R_list_threads !l
  | Commands.Request.R_get_thread { tid } ->
    with_find_thread_state_ self.cur_proc ~tid @@ ( function
    | None -> Res.R_error "thread not found"
    | Some t -> Res.R_thread (get_t_state t) )
  | Commands.Request.R_get_all_threads ->
    let l = ref [] in
    Tid_tbl.iter self.cur_proc.threads (fun _ (t : thread_state) ->
        l := get_t_state t :: !l);
    Res.R_all_threads !l
  | Commands.Request.R_get_counter { name } ->
    Counter_tbl.with_find self.counters name @@ ( function
    | None -> Res.R_error "span not found"
    | Some c -> Res.R_counter (get_counter c) )
  | Commands.Request.R_get_all_counters ->
    let l = ref [] in
    Counter_tbl.iter self.counters (fun _ c -> l := get_counter c :: !l);
    Res.R_all_counters !l
