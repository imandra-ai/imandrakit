open Common_
module DAP = Imandrakit_debug_dap

type t = {
  d: Debug.t;
  out_seq: int Atomic.t;
  sock: Unix.file_descr;
  port: int;
}

let port self = self.port

let send_msg_ ~(oc : out_channel Lock.t) (msg : Json.t) : unit =
  let@ oc = Lock.with_lock oc in
  Wire.write oc msg

let mk_event_ (self : t) ~type_ ?body () : DAP.Event.t =
  let seq = Atomic.fetch_and_add self.out_seq 1 in
  DAP.Event.make ~seq ~type_:Event ~event:type_ ?body ()

let mk_response_ (self : t) ~type_ ~req_seq ~success ?body () : DAP.Response.t =
  let seq = Atomic.fetch_and_add self.out_seq 1 in
  DAP.Response.make ~seq ~request_seq:req_seq ~type_:Response ~command:type_
    ~success ?body ()

(** Send event to a connected DAP client *)
let send_notif (self : t) ~oc (cmd : Commands.Server_notification.t) : unit =
  Log.debug (fun k -> k "send notif %a" Commands.Server_notification.pp cmd);

  (* send the event as a DAP message *)
  (match cmd with
  | N_enabled { level = _ } ->
    send_msg_ ~oc @@ DAP.Event.to_yojson
    @@ mk_event_ self ~type_:DAP.Initialized_event.type_ ()
  | N_interrupted { tid } ->
    send_msg_ ~oc @@ DAP.Event.to_yojson
    @@ mk_event_ self ~type_:DAP.Stopped_event.type_
         ~body:
           DAP.Stopped_event.Payload.(
             make ~thread_id:(Some tid) ~reason:Breakpoint () |> to_yojson)
         ()
  | N_interrupted_all ->
    send_msg_ ~oc @@ DAP.Event.to_yojson
    @@ mk_event_ self ~type_:DAP.Stopped_event.type_
         ~body:
           DAP.Stopped_event.Payload.(
             make ~thread_id:None ~all_threads_stopped:(Some true)
               ~reason:Breakpoint ()
             |> to_yojson)
         ()
  | N_disabled | N_events _ | N_disconnect -> ());

  ()

open struct
  let unwrap_j = function
    | Ok x -> x
    | Error j -> Error.failf ~kind:debug_error "Invalid json: %s" j
end

let handle_req_ (self : t) ~oc (_client : Debug.Client_state.t)
    (req : DAP.Request.t) : unit =
  Log.debug (fun k -> k "handle req %a" Json.pp (DAP.Request.to_yojson req));
  match req.command with
  | "pause" ->
    let pause =
      DAP.Pause_command.Arguments.of_yojson req.arguments |> unwrap_j
    in
    let ok = Debug.suspend_thread self.d ~tid:pause.thread_id in
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq ~type_:DAP.Pause_command.type_
         ~success:ok ()
  | _ ->
    Log.warn (fun k -> k "unhandled req %a" Json.pp (DAP.Request.to_yojson req));
    ()

let client_handle_loop_ (self : t) ~ic ~(oc : out_channel Lock.t)
    (client : Debug.Client_state.t) : unit =
  while Debug.Client_state.is_active client do
    match Wire.read ic with
    | Error err ->
      Log.err (fun k -> k "wire protocol error: %a" Error.pp err);
      Debug.remove_client self.d client
    | Ok None -> Debug.remove_client self.d client
    | Ok (Some j) ->
      let msg = DAP.Protocol_message.of_yojson j |> unwrap_j in
      (match msg.type_ with
      | Request ->
        let req = DAP.Request.of_yojson j |> unwrap_j in
        handle_req_ self ~oc client req
      | Response -> ()
      | Event | DAP.Protocol_message.Type.Custom _ -> ())
  done

let listen_loop_ (self : t) : unit =
  while true do
    let sock, _client_addr = Unix.accept self.sock in
    let ic = Unix.in_channel_of_descr sock in
    let oc = Lock.create @@ Unix.out_channel_of_descr sock in
    let client_st = Debug.add_client self.d ~on_notif:(send_notif self ~oc) in
    ignore
      (Thread.create (client_handle_loop_ self ~ic ~oc) client_st : Thread.t)
  done

let try_bind sock port : (int, string) result =
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  try
    Unix.bind sock addr;
    match Unix.getsockname sock with
    | Unix.ADDR_INET (_, p) -> Ok p
    | Unix.ADDR_UNIX _ -> assert false
  with Unix.Unix_error (err, _, _) -> Error (Unix.error_message err)

let setup_ ~port () : Thread.t =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.TCP_NODELAY true;

  (* bind socket *)
  let port =
    match try_bind sock port with
    | Ok p -> p
    | Error _ when port <> 0 ->
      (match try_bind sock 0 with
      | Ok p -> p
      | Error msg ->
        Error.failf ~kind:debug_error "Cannot bind on port=%d: %s" port msg)
    | Error msg ->
      Error.failf ~kind:debug_error "Cannot bind on port=%d: %s" port msg
  in

  Unix.listen sock 8;

  let st = { port; d = Debug.global_st; sock; out_seq = Atomic.make 0 } in
  Thread.create listen_loop_ st

let bg_thread : Thread.t option Immlock.t = Immlock.create None

let setup ?(port = 0) () =
  (* see if we need to setup the background thread *)
  Immlock.update bg_thread (function
    | Some _ as st -> st
    | None ->
      (match setup_ ~port () with
      | t -> Some t
      | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        Error.raise_err ~bt
        @@ Error.of_exn ~kind:Error_kind.generic_internal_error ~bt exn))
