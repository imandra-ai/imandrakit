open Common_
module DAP = Imandrakit_debug_dap

let default_port = 19525

type t = {
  d: Debug.t;
  out_seq: int Atomic.t;
  sock: Unix.file_descr;
  port: int;
  session_args: DAP.Initialize_command.Arguments.t Int_tbl.t;
  mutable saw_attach_commands: bool;
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

let dap_config () : DAP.Initialize_command.Result.t =
  {
    supports_configuration_done_request = Some true;
    supports_function_breakpoints = Some false;
    supports_conditional_breakpoints = Some false;
    supports_hit_conditional_breakpoints = Some false;
    supports_evaluate_for_hovers = Some true;
    exception_breakpoint_filters = None;
    supports_step_back = Some false;
    supports_set_variable = Some false;
    supports_restart_frame = Some false;
    supports_goto_targets_request = Some false;
    supports_step_in_targets_request = Some false;
    supports_completions_request = Some false;
    completion_trigger_characters = Some [ "." ];
    supports_modules_request = Some false;
    additional_module_columns = None;
    supported_checksum_algorithms = None;
    supports_restart_request = None;
    supports_exception_options = None;
    supports_value_formatting_options = None;
    supports_exception_info_request = Some false;
    support_terminate_debuggee = Some true;
    supports_delayed_stack_trace_loading = Some true;
    supports_loaded_sources_request = None;
    supports_log_points = None;
    supports_terminate_threads_request = None;
    supports_set_expression = Some false;
    supports_terminate_request = None;
    supports_data_breakpoints = Some false;
    supports_read_memory_request = Some false;
    supports_disassemble_request = Some false;
    supports_cancel_request = Some true;
    supports_breakpoint_locations_request = Some true;
    supports_clipboard_context = None;
    supports_stepping_granularity = Some false;
    supports_instruction_breakpoints = Some false;
    supports_exception_filter_options = Some false;
  }

let handle_req_ (self : t) ~oc (client : Debug.Client_state.t)
    (req : DAP.Request.t) : unit =
  (* Log.debug (fun k ->
      k "handle req: %s" (Json.to_string (DAP.Request.to_yojson req))); *)
  match req.command with
  | "pause" ->
    let args =
      DAP.Pause_command.Arguments.of_yojson req.arguments |> unwrap_j
    in
    let ok = Debug.suspend_thread self.d ~tid:args.thread_id in
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq ~type_:DAP.Pause_command.type_
         ~success:ok ();
    send_msg_ ~oc @@ DAP.Event.to_yojson
    @@ mk_event_ self ~type_:DAP.Stopped_event.type_
         ~body:
           (DAP.Stopped_event.Payload.to_yojson
              {
                reason = DAP.Stopped_event.Payload.Reason.Pause;
                description = Some "paused";
                thread_id = Some args.thread_id;
                preserve_focus_hint = None;
                text = None;
                all_threads_stopped = Some true;
              })
         ()
  | "initialize" ->
    let args =
      DAP.Initialize_command.Arguments.of_yojson req.arguments |> unwrap_j
    in
    Int_tbl.add self.session_args (Debug.Client_state.id client) args;

    (* When blocking a thread, we will issue a break event. *)
    Debug.set_on_block self.d (fun tid ->
        send_msg_ ~oc @@ DAP.Event.to_yojson
        @@ mk_event_ self ~type_:DAP.Stopped_event.type_
             ~body:
               (DAP.Stopped_event.Payload.to_yojson
                  {
                    reason = DAP.Stopped_event.Payload.Reason.Breakpoint;
                    description = Some "Breakpoint";
                    thread_id = Some tid;
                    preserve_focus_hint = None;
                    text = None;
                    all_threads_stopped = Some true;
                  })
             ());

    let cfg = dap_config () in
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq ~type_:DAP.Initialize_command.type_
         ~success:true
         ~body:(DAP.Initialize_command.Result.to_yojson cfg)
         ();
    (* Immediately send the initialized event as well. *)
    send_msg_ ~oc @@ DAP.Event.to_yojson
    @@ mk_event_ self ~type_:DAP.Initialized_event.type_ ()
  | "launch" ->
    let _args =
      DAP.Launch_command.Arguments.of_yojson req.arguments |> unwrap_j
    in
    (* TODO: remember args for later? *)
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq ~type_:DAP.Launch_command.type_
         ~success:true
         ~body:(DAP.Launch_command.Result.to_yojson ())
         ()
  | "attach" ->
    let _args =
      DAP.Attach_command.Arguments.of_yojson req.arguments |> unwrap_j
    in
    (* Not supported (yet), so we report failure. *)
    self.saw_attach_commands <- true;
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq ~type_:DAP.Attach_command.type_
         ~success:false ()
  | "setBreakpoints" ->
    let _args =
      DAP.Set_breakpoints_command.Arguments.of_yojson req.arguments |> unwrap_j
    in
    (* TODO: anything to do with breakpoints? *)
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq
         ~type_:DAP.Set_breakpoints_command.type_ ~success:true
         ~body:
           (DAP.Set_breakpoints_command.Result.to_yojson
              { breakpoints = [ (* TODO *) ] })
         ()
  | "setFunctionBreakpoints" ->
    let _args =
      DAP.Set_function_breakpoints_command.Arguments.of_yojson req.arguments
      |> unwrap_j
    in
    (* TODO: anything to do with breakpoints? *)
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq
         ~type_:DAP.Set_function_breakpoints_command.type_ ~success:true
         ~body:
           (DAP.Set_function_breakpoints_command.Result.to_yojson
              { breakpoints = [ (* TODO *) ] })
         ()
  | "setInstructionBreakpoints" ->
    let _args =
      DAP.Set_instruction_breakpoints_command.Arguments.of_yojson req.arguments
      |> unwrap_j
    in
    (* TODO: anything to do with breakpoints? *)
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq
         ~type_:DAP.Set_instruction_breakpoints_command.type_ ~success:true
         ~body:
           (DAP.Set_instruction_breakpoints_command.Result.to_yojson
              { breakpoints = [ (* TODO *) ] })
         ()
  | "setDataBreakpoints" ->
    let _args =
      DAP.Set_data_breakpoints_command.Arguments.of_yojson req.arguments
      |> unwrap_j
    in
    (* TODO: anything to do with breakpoints? *)
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq
         ~type_:DAP.Set_data_breakpoints_command.type_ ~success:true
         ~body:
           (DAP.Set_data_breakpoints_command.Result.to_yojson
              { breakpoints = [ (* TODO *) ] })
         ()
  | "configurationDone" ->
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq
         ~type_:DAP.Configuration_done_command.type_ ~success:true
         ~body:(DAP.Configuration_done_command.Result.to_yojson ())
         ();
    if Debug.Private_.is_blocked () then
      (* We could have run into a breakpoint/suspension before the
         session was initialized, so we need to send a Stopped event now. *)
      send_msg_ ~oc @@ DAP.Event.to_yojson
      @@ mk_event_ self ~type_:DAP.Stopped_event.type_
           ~body:
             (DAP.Stopped_event.Payload.to_yojson
                {
                  reason = DAP.Stopped_event.Payload.Reason.Entry;
                  description = Some "Stop on entry";
                  thread_id = Some 0;
                  preserve_focus_hint = Some false;
                  text = None;
                  all_threads_stopped = Some true;
                })
           ()
  | "threads" ->
    let ts = Debug.threads self.d in
    let ts =
      List.map
        (fun (t : Debug.thread_info) -> { DAP.Thread.id = t.id; name = t.name })
        ts
    in
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq ~type_:DAP.Threads_command.type_
         ~success:true
         ~body:(DAP.Threads_command.Result.to_yojson { threads = ts })
         ()
  | "stackTrace" ->
    let args =
      DAP.Stack_trace_command.Arguments.of_yojson req.arguments |> unwrap_j
    in
    let stack_frames, num_available =
      Debug.stack_frames self.d args.thread_id args.start_frame args.levels
    in
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq ~type_:DAP.Stack_trace_command.type_
         ~success:true
         ~body:
           (DAP.Stack_trace_command.Result.to_yojson
              {
                stack_frames =
                  List.map
                    (fun (f : Debug.stack_frame) ->
                      {
                        DAP.Stack_frame.id = f.id;
                        name = f.name;
                        source =
                          Some
                            {
                              name = Some (Filename.basename f.source.path);
                              path =
                                Some
                                  (try Unix.realpath f.source.path
                                   with _ -> f.source.path);
                              source_reference = None;
                              presentation_hint = None;
                              origin = None;
                              sources = None;
                              adapter_data = None;
                              checksums = None;
                            };
                        line = f.source.line;
                        column = f.source.column;
                        end_line = None;
                        end_column = None;
                        instruction_pointer_reference = None;
                        module_id = None;
                        presentation_hint = None;
                      })
                    stack_frames;
                total_frames = Some num_available;
              })
         ()
  | "source" ->
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq ~type_:DAP.Source_command.type_
         ~success:true
         ~body:
           (DAP.Source_command.Result.to_yojson
              { content = "my source code"; mime_type = None })
         ()
  | "scopes" ->
    let args =
      DAP.Scopes_command.Arguments.of_yojson req.arguments |> unwrap_j
    in
    let scopes = Debug.scopes self.d args.frame_id in
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq ~type_:DAP.Scopes_command.type_
         ~success:true
         ~body:
           (DAP.Scopes_command.Result.to_yojson
              {
                scopes =
                  List.map
                    (fun (s : Debug.scope) ->
                      {
                        DAP.Scope.name = s.name;
                        presentation_hint = None;
                        variables_reference = s.variables_reference;
                        named_variables = None;
                        indexed_variables = None;
                        expensive = false;
                        source = None;
                        line = None;
                        column = None;
                        end_line = None;
                        end_column = None;
                      })
                    scopes;
              })
         ()
  | "evaluate" ->
    let args =
      DAP.Evaluate_command.Arguments.of_yojson req.arguments |> unwrap_j
    in
    let rsp =
      match Debug.evaluate self.d args.frame_id args.expression with
      | Some (v, vr) ->
        mk_response_ self ~req_seq:req.seq ~type_:DAP.Evaluate_command.type_
          ~success:true
          ~body:
            (DAP.Evaluate_command.Result.to_yojson
               {
                 result = v;
                 type_ = None;
                 presentation_hint = None;
                 variables_reference = vr;
                 named_variables = None;
                 indexed_variables = None;
                 memory_reference = None;
               })
          ()
      | None ->
        mk_response_ self ~req_seq:req.seq ~type_:DAP.Evaluate_command.type_
          ~success:false ()
    in
    send_msg_ ~oc @@ DAP.Response.to_yojson @@ rsp
  | "variables" ->
    let args =
      DAP.Variables_command.Arguments.of_yojson req.arguments |> unwrap_j
    in
    let vars : Debug.variable list =
      Debug.variables self.d args.variables_reference
    in
    let include_type =
      match
        Int_tbl.find_opt self.session_args (Debug.Client_state.id client)
      with
      | Some x -> Option.value x.supports_variable_type ~default:false
      | None -> false
    in
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq ~type_:DAP.Variables_command.type_
         ~success:true
         ~body:
           (DAP.Variables_command.Result.to_yojson
              {
                variables =
                  List.map
                    (fun (v : Debug.variable) ->
                      {
                        DAP.Variable.name = v.name;
                        value = v.value;
                        type_ =
                          (if include_type then
                             Some v.type_
                           else
                             None);
                        presentation_hint = None;
                        evaluate_name = None;
                        variables_reference = v.variables_reference;
                        named_variables = None;
                        indexed_variables = None;
                        memory_reference = None;
                        __vscode_variable_menu_context = None;
                      })
                    vars;
              })
         ()
  | "breakpointLocations" ->
    let _args =
      DAP.Breakpoint_locations_command.Arguments.of_yojson req.arguments
      |> unwrap_j
    in
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq
         ~type_:DAP.Breakpoint_locations_command.type_ ~success:true
         ~body:
           (DAP.Breakpoint_locations_command.Result.to_yojson
              {
                breakpoints =
                  [ (* TODO: is there any way we could report where the breakpoints are? *) ];
              })
         ()
  | "continue" ->
    let _args =
      DAP.Continue_command.Arguments.of_yojson req.arguments |> unwrap_j
    in
    Debug.Private_.continue self.d;
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq ~type_:DAP.Continue_command.type_
         ~success:true
         ~body:
           (DAP.Continue_command.Result.to_yojson
              { all_threads_continued = Some true })
         ()
  | "next" ->
    let _args =
      DAP.Next_command.Arguments.of_yojson req.arguments |> unwrap_j
    in
    (* TODO: This is supposed to wake only the thread in args.thread_id, not all of them. *)
    Debug.Private_.continue self.d;
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq ~type_:DAP.Next_command.type_
         ~success:true
         ~body:(DAP.Next_command.Result.to_yojson ())
         ()
  | "stepIn" ->
    (* Not supported and disabled in the config. *)
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq ~type_:DAP.Step_in_command.type_
         ~success:false ()
  | "stepOut" ->
    (* Not supported and disabled in the config. *)
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq
         ~type_:DAP.Step_in_targets_command.type_ ~success:false ()
  | "stepInTargets" ->
    (* Not supported and disabled in the config. *)
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq
         ~type_:DAP.Step_in_targets_command.type_ ~success:false ()
  | "stepBack" ->
    (* Not supported and disabled in the config. *)
    send_msg_ ~oc @@ DAP.Response.to_yojson
    @@ mk_response_ self ~req_seq:req.seq ~type_:DAP.Step_back_command.type_
         ~success:false ()
  | "disconnect" ->
    let disconnect_args =
      DAP.Disconnect_command.Arguments.of_yojson req.arguments |> unwrap_j
    in
    Log.debug (fun k ->
        k "(disconnect :restart %a :terminate_debuggee %a)"
          (Fmt.Dump.option Fmt.bool) disconnect_args.restart
          (Fmt.Dump.option Fmt.bool) disconnect_args.terminate_debuggee);
    Int_tbl.remove self.session_args (Debug.Client_state.id client);
    Debug.remove_client self.d client;
    if not self.saw_attach_commands then
      (* TODO: we need to distinguish one-shot and (multi-)attachable debuggers.
         For now, if all we ever saw was a launch command, but no attach
         commands, then we assume that we should terminate. *)
      exit 0
  | _ ->
    Log.warn (fun k ->
        k "unhandled req %s" (Json.to_string (DAP.Request.to_yojson req)));
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
    | Ok p ->
      Printf.printf "%d\n%!" p;
      p
    | Error _ when port <> 0 ->
      (match try_bind sock 0 with
      | Ok p ->
        Printf.printf "%d\n%!" p;
        p
      | Error msg ->
        Error.failf ~kind:debug_error "Cannot bind on port=%d: %s" port msg)
    | Error msg ->
      Error.failf ~kind:debug_error "Cannot bind on port=%d: %s" port msg
  in

  Unix.listen sock 8;

  let st =
    {
      port;
      d = Debug.global_st;
      sock;
      out_seq = Atomic.make 1;
      session_args = Int_tbl.create 1;
      saw_attach_commands = false;
    }
  in
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
