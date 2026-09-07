module Trace = Trace_core
module Log = (val Logger.mk_log_str "x.trace-async")

type span_id = int64
type trace_id = string

let dummy_span = 0L
let dummy_trace_id : trace_id = ""

type explicit_span = {
  span: span_id;
      (** Identifier for this span. Several explicit spans might share the same
          identifier since we can differentiate between them via [meta]. *)
  trace_id: trace_id;  (** The trace this belongs to *)
  mutable meta: unit;
      (** Metadata for this span (and its context). This can be used by
          collectors to carry collector-specific information from the beginning
          of the span, to the end of the span. *)
}

(** Current parent scope for async spans *)
let k_span_ctx : Trace.span Hmap.key = Hmap.Key.create ()

open struct
  let auto_enrich_span_l_ : (Trace.span -> unit) list Atomic.t = Atomic.make []

  let with_span_real_ ~level ~(parent : Trace.span option) ?data ?__FUNCTION__
      ~__FILE__ ~__LINE__ name (f : Trace.span -> 'a) : 'a =
    let parent =
      match parent with
      | None -> Trace.current_span ()
      | Some x when x = Trace.Collector.dummy_span -> Trace.current_span ()
      | _ -> parent
    in

    let span =
      Trace.enter_span ~parent ~flavor:`Async ?data ~level ?__FUNCTION__
        ~__FILE__ ~__LINE__ name
    in

    let@ _ = Trace.with_current_span_set_to span in

    (* apply automatic enrichment *)
    if span != Trace.Collector.dummy_span then
      List.iter (fun f -> f span) (Atomic.get auto_enrich_span_l_);

    try
      let x = f span in
      Trace.exit_span span;
      x
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      Opentelemetry_trace.record_exception span e bt;
      Trace.exit_span span;
      Printexc.raise_with_backtrace e bt
end

(** Wrap [f()] in a async span. *)
let with_span ?(level = Trace.get_default_level ())
    ?(parent : Trace.span option) ?data ?__FUNCTION__ ~__FILE__ ~__LINE__ name
    (f : Trace.span -> 'a) : 'a =
  let trace_enabled = Trace.enabled () in
  if trace_enabled && level <= Trace.get_current_level () then
    with_span_real_ ~level ~parent ?data ?__FUNCTION__ ~__FILE__ ~__LINE__ name
      f
  else (
    match parent with
    | Some p when trace_enabled ->
      (* make sure we still link spans in [f()] to [p] *)
      let@ _ = Trace.with_current_span_set_to p in
      f Trace.Collector.dummy_span
    | _ -> f Trace.Collector.dummy_span
  )

let with_fresh_trace ~level ?data ?__FUNCTION__ ~__FILE__ ~__LINE__ name
    (f : Trace.span -> 'a) : 'a =
  match Opentelemetry.Sdk.get () with
  | Some exporter ->
    let trace_id = Opentelemetry.Trace_id.create () in
    let id = Opentelemetry.Span_id.create () in
    let sctx = Opentelemetry.Span_ctx.make ~trace_id ~parent_id:id () in
    let otel_parent =
      let start_time =
        Opentelemetry.Clock.now
          (Opentelemetry.Exporter.get_tracer exporter).clock
      in
      Opentelemetry.Span.make ~trace_id ~id ~start_time ~end_time:start_time
        name
    in
    let parent = Opentelemetry_trace.Extensions.Span_otel otel_parent in
    Fun.protect
      ~finally:(fun () ->
        Trace.exit_span parent
        (* Opentelemetry.Emitter.emit
          (Opentelemetry.Exporter.get_tracer exporter).emit [ otel_parent ] *))
      (fun () ->
        with_span ~level ~parent ?data ?__FUNCTION__ ~__FILE__ ~__LINE__ name f)
  | _ -> f Trace.Collector.dummy_span

open struct
  let cons_assoc_opt_ name x l =
    match x with
    | None -> l
    | Some v -> (name, `String v) :: l
end

let add_data_to_span (span : Trace.span) data : unit =
  (* Trace.add_data_to_span (IK (span_to_explicit_span span)) data *)
  Trace.add_data_to_span span data

let enrich_span_service ?version (span : Trace.span) : unit =
  let data = [] |> cons_assoc_opt_ "service.version" version in
  Trace.add_data_to_span span data

let enrich_span_deployment ?id ?name ~deployment (span : Trace.span) : unit =
  let data =
    [ "deployment.environment.name", `String deployment ]
    |> cons_assoc_opt_ "deployment.id" id
    |> cons_assoc_opt_ "deployment.name" name
  in
  add_data_to_span span data

(** Add a hook that will be called on every explicit span *)
let add_auto_enrich_span (f : Trace.span -> unit) : unit =
  while
    let l = Atomic.get auto_enrich_span_l_ in
    not (Atomic.compare_and_set auto_enrich_span_l_ l (f :: l))
  do
    ()
  done
