module Trace = Trace_core
module LS = Moonpool.Task_local_storage

(** Current parent scope for async spans *)
let k_parent_scope : Trace.explicit_span option LS.key =
  LS.new_key ~init:(fun () -> None) ()

(** Set the parent scope by hand *)
let set_parent_scope (sp : Trace.explicit_span) =
  LS.set k_parent_scope (Some sp)

let add_exn_to_span (sp : Trace.explicit_span) (exn : exn) =
  Trace.add_data_to_manual_span sp
    [ "exception.message", `String (Printexc.to_string exn) ]

let add_bt_to_span (sp : Trace.explicit_span) (bt : Printexc.raw_backtrace) =
  Trace.add_data_to_manual_span sp
    [ "exception.stacktrace", `String (Printexc.raw_backtrace_to_string bt) ]

open struct
  let with_span_real_ ~level ?data ?__FUNCTION__ ~__FILE__ ~__LINE__ name f =
    let storage = LS.get_current () in
    let parent =
      CCOption.flat_map
        (fun store -> LS.Direct.get store k_parent_scope)
        storage
    in
    let span =
      match parent with
      | None ->
        Trace.enter_manual_toplevel_span ~flavor:`Async ?data ~level
          ?__FUNCTION__ ~__FILE__ ~__LINE__ name
      | Some parent ->
        Trace.enter_manual_sub_span ~parent ~flavor:`Async ?data ~level
          ?__FUNCTION__ ~__FILE__ ~__LINE__ name
    in

    (* set current span as parent, for children *)
    LS.set k_parent_scope (Some span);

    (* cleanup *)
    let finally () =
      (* restore previous parent span *)
      Option.iter
        (fun store -> LS.Direct.set store k_parent_scope parent)
        storage;
      Trace.exit_manual_span span
    in

    try
      let x = f span in
      finally ();
      x
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      add_exn_to_span span e;
      add_bt_to_span span bt;
      Printexc.raise_with_backtrace e bt
end

(** Wrap [f()] in a async span. *)
let with_span ?(level = Trace.get_default_level ()) ?data ?__FUNCTION__
    ~__FILE__ ~__LINE__ name (f : Trace.explicit_span -> 'a) : 'a =
  if Trace.enabled () && level <= Trace.get_current_level () then
    with_span_real_ ~level ?data ?__FUNCTION__ ~__FILE__ ~__LINE__ name f
  else
    f Trace.Collector.dummy_explicit_span
