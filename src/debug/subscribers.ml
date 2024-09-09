module E = Event
module Sub = Trace_subscriber

let emit (Sub { st; callbacks = (module CB) } : Sub.t) (e : E.t) : unit =
  match e with
  | E_init { time_ns } -> CB.on_init st ~time_ns
  | E_shutdown { time_ns } -> CB.on_shutdown st ~time_ns
  | E_name_thread { time_ns; tid; name } ->
    CB.on_name_thread st ~time_ns ~tid ~name
  | E_name_process { time_ns; tid; name } ->
    CB.on_name_process st ~time_ns ~tid ~name
  | E_enter_span
      { __FUNCTION__; __FILE__; __LINE__; time_ns; tid; data; name; span } ->
    CB.on_enter_span st ~__FUNCTION__ ~__FILE__ ~__LINE__ ~time_ns ~tid ~data
      ~name span
  | E_exit_span { time_ns; tid; span } -> CB.on_exit_span st ~time_ns ~tid span
  | E_add_data { data; span } -> CB.on_add_data st ~data span
  | E_message { time_ns; tid; span; data; msg } ->
    CB.on_message st ~time_ns ~tid ~span ~data msg
  | E_counter { time_ns; tid; data; name; n } ->
    CB.on_counter st ~time_ns ~tid ~data ~name n
  | E_enter_manual_span
      {
        __FUNCTION__;
        __FILE__;
        __LINE__;
        time_ns;
        tid;
        parent;
        data;
        name;
        flavor;
        trace_id;
        span;
      } ->
    CB.on_enter_manual_span st ~__FUNCTION__ ~__FILE__ ~__LINE__ ~time_ns ~tid
      ~parent ~data ~name ~flavor ~trace_id span
  | E_exit_manual_span { time_ns; tid; name; data; flavor; trace_id; span } ->
    CB.on_exit_manual_span st ~time_ns ~tid ~name ~data ~flavor ~trace_id span

let to_events (type st0) ?(shutdown = ignore) ~emit_ev (st0 : st0) : Sub.t =
  let module M = struct
    type st = {
      st0: st0;
      emit_ev: st0 -> E.t -> unit;
      shutdown: st0 -> unit;
    }

    let on_init st ~time_ns = st.emit_ev st.st0 @@ E_init { time_ns }
    let on_shutdown st ~time_ns = st.emit_ev st.st0 @@ E_shutdown { time_ns }

    let on_name_thread st ~time_ns ~tid ~name =
      st.emit_ev st.st0 @@ E_name_thread { time_ns; tid; name }

    let on_name_process st ~time_ns ~tid ~name =
      st.emit_ev st.st0 @@ E_name_process { time_ns; tid; name }

    let on_enter_span st ~__FUNCTION__ ~__FILE__ ~__LINE__ ~time_ns ~tid ~data
        ~name span =
      st.emit_ev st.st0
      @@ E_enter_span
           { __FUNCTION__; __FILE__; __LINE__; time_ns; tid; data; name; span }
    (*
  | E_exit_span { time_ns; tid; span } -> CB.on_exit_span st ~time_ns ~tid span
  | E_add_data { data; span } -> CB.on_add_data st ~data span
  | E_message { time_ns; tid; span; data; msg } ->
    CB.on_message st ~time_ns ~tid ~span ~data msg
  | E_counter { time_ns; tid; data; name; n } ->
    CB.on_counter st ~time_ns ~tid ~data ~name n
  | E_enter_manual_span
      {
        __FUNCTION__;
        __FILE__;
        __LINE__;
        time_ns;
        tid;
        parent;
        data;
        name;
        flavor;
        trace_id;
        span;
      } ->
    CB.on_enter_manual_span st ~__FUNCTION__ ~__FILE__ ~__LINE__ ~time_ns ~tid
      ~parent ~data ~name ~flavor ~trace_id span
  | E_exit_manual_span { time_ns; tid; name; data; flavor; trace_id; span } ->
    CB.on_exit_manual_span st ~time_ns ~tid ~name ~data ~flavor ~trace_id span
  *)
  end in
  let st : M.st = { st0; emit_ev; shutdown } in
  Sub.Subscriber.Sub { st; callbacks = (module M) }
