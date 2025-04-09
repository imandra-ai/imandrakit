module Trace = Trace_core

type Trace.extension_event +=
  | Ev_link_span of Trace.explicit_span * Trace.explicit_span_ctx
        (** Link the given span to the given context. The context isn't the
            parent, but the link can be used to correlate both spans. *)
  | Ev_record_exn of {
      sp: Trace.span;
      exn: exn;
      bt: Printexc.raw_backtrace;
      error: bool;  (** Is this an actual internal error? *)
    }
        (** Record exception and potentially turn span to an error *)
  | Ev_push_async_parent of Trace.explicit_span_ctx
        (** Set current async span *)
  | Ev_pop_async_parent of Trace.explicit_span_ctx
        (** Remove current async span *)

(** Link the given span to the given context *)
let[@inline] link_spans (sp1 : Trace.explicit_span)
    ~(src : Trace.explicit_span_ctx) : unit =
  if Trace.enabled () then Trace.extension_event @@ Ev_link_span (sp1, src)

(** Current parent scope for async spans *)
let k_span_ctx : Trace.explicit_span_ctx Hmap.key = Hmap.Key.create ()

(** Record exception in the span *)
let add_exn_to_span ~is_error (sp : Trace.span) (exn : exn)
    (bt : Printexc.raw_backtrace) =
  Trace.extension_event @@ Ev_record_exn { sp; exn; bt; error = is_error }

let push_async_parent (sp : Trace.explicit_span_ctx) : unit =
  Trace.extension_event @@ Ev_push_async_parent sp

let pop_async_parent (sp : Trace.explicit_span_ctx) : unit =
  Trace.extension_event @@ Ev_pop_async_parent sp

let[@inline] with_async_parent (sp : Trace.explicit_span_ctx) f =
  push_async_parent sp;
  Fun.protect ~finally:(fun () -> pop_async_parent sp) f

open struct
  let auto_enrich_span_l_ : (Trace.explicit_span -> unit) list Atomic.t =
    Atomic.make []

  let with_span_real_ ~level ?parent ?data ?__FUNCTION__ ~__FILE__ ~__LINE__
      name (f : Trace_core.explicit_span * Trace_core.explicit_span_ctx -> 'a) :
      'a =
    let span =
      Trace.enter_manual_span ~parent ~flavor:`Async ?data ~level ?__FUNCTION__
        ~__FILE__ ~__LINE__ name
    in

    (* apply automatic enrichment *)
    if span.span != Trace.Collector.dummy_span then
      List.iter (fun f -> f span) (Atomic.get auto_enrich_span_l_);

    try
      let x = f (span, Trace.ctx_of_span span) in
      Trace.exit_manual_span span;
      x
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      add_exn_to_span ~is_error:true span.span e bt;
      Trace.exit_manual_span span;
      Printexc.raise_with_backtrace e bt
end

(** Wrap [f()] in a async span. *)
let with_span ?(level = Trace.get_default_level ()) ?parent ?data ?__FUNCTION__
    ~__FILE__ ~__LINE__ name
    (f : Trace.explicit_span * Trace.explicit_span_ctx -> 'a) : 'a =
  if Trace.enabled () && level <= Trace.get_current_level () then
    with_span_real_ ~level ?parent ?data ?__FUNCTION__ ~__FILE__ ~__LINE__ name
      f
  else (
    let span_ctx =
      match parent with
      | None -> Trace.Collector.dummy_explicit_span_ctx
      | Some p -> p
    in
    f (Trace.Collector.dummy_explicit_span, span_ctx)
  )

open struct
  let cons_assoc_opt_ name x l =
    match x with
    | None -> l
    | Some v -> (name, `String v) :: l
end

let enrich_span_service ?version (span : Trace.explicit_span) : unit =
  let data = [] |> cons_assoc_opt_ "service.version" version in
  Trace.add_data_to_manual_span span data

let enrich_span_deployment ?id ?name ~deployment (span : Trace.explicit_span) :
    unit =
  let data =
    [ "deployment.environment.name", `String deployment ]
    |> cons_assoc_opt_ "deployment.id" id
    |> cons_assoc_opt_ "deployment.name" name
  in
  Trace.add_data_to_manual_span span data

(** Add a hook that will be called on every explicit span *)
let add_auto_enrich_span (f : Trace.explicit_span -> unit) : unit =
  while
    let l = Atomic.get auto_enrich_span_l_ in
    not (Atomic.compare_and_set auto_enrich_span_l_ l (f :: l))
  do
    ()
  done
