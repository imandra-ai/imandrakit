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

module To_events_callbacks = struct
  open E

  (** State and callback *)
  type st =
    | St : {
        st: 'a;
        emit_ev: 'a -> E.t -> unit;
      }
        -> st

  let on_init (St st) ~time_ns = st.emit_ev st.st @@ E_init { time_ns }
  let on_shutdown (St st) ~time_ns = st.emit_ev st.st @@ E_shutdown { time_ns }

  let on_name_thread (St st) ~time_ns ~tid ~name =
    st.emit_ev st.st @@ E_name_thread { time_ns; tid; name }

  let on_name_process (St st) ~time_ns ~tid ~name =
    st.emit_ev st.st @@ E_name_process { time_ns; tid; name }

  let on_enter_span (St st) ~__FUNCTION__ ~__FILE__ ~__LINE__ ~time_ns ~tid
      ~data ~name span =
    st.emit_ev st.st
    @@ E_enter_span
         { __FUNCTION__; __FILE__; __LINE__; time_ns; tid; data; name; span }

  let on_exit_span (St st) ~time_ns ~tid span =
    st.emit_ev st.st @@ E_exit_span { time_ns; tid; span }

  let on_add_data (St st) ~data span =
    st.emit_ev st.st @@ E_add_data { data; span }

  let on_message (St st) ~time_ns ~tid ~span ~data msg =
    st.emit_ev st.st @@ E_message { time_ns; tid; span; data; msg }

  let on_counter (St st) ~time_ns ~tid ~data ~name n =
    st.emit_ev st.st @@ E_counter { time_ns; tid; data; name; n }

  let on_enter_manual_span (St st) ~__FUNCTION__ ~__FILE__ ~__LINE__ ~time_ns
      ~tid ~parent ~data ~name ~flavor ~trace_id span =
    st.emit_ev st.st
    @@ E_enter_manual_span
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
         }

  let on_exit_manual_span (St st) ~time_ns ~tid ~name ~data ~flavor ~trace_id
      span =
    st.emit_ev st.st
    @@ E_exit_manual_span { time_ns; tid; name; data; flavor; trace_id; span }
end

let to_events (type st) ~emit_ev (st : st) : Sub.t =
  Sub.Subscriber.Sub
    {
      st = To_events_callbacks.St { st; emit_ev };
      callbacks = (module To_events_callbacks);
    }
