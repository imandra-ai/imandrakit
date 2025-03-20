module Trace = Trace_core
module LS = Moonpool.Task_local_storage

(** Current parent scope for async spans *)
let k_parent_scope : Trace.explicit_span Hmap.key = Hmap.Key.create ()

(** Set the parent scope by hand *)
let[@inline] set_parent_scope (sp : Trace.explicit_span) =
  try LS.set_in_local_hmap k_parent_scope sp with _ -> ()

let[@inline] get_parent_scope () : Trace.explicit_span option =
  try LS.get_in_local_hmap_opt k_parent_scope with _ -> None

let add_exn_to_span (sp : Trace.explicit_span) (exn : exn)
    (bt : Printexc.raw_backtrace) =
  let msg = Printexc.to_string exn in
  Trace.add_data_to_manual_span sp
    [
      (* mark the status as failed *)
      "otrace.error", `String msg;
      "exception.message", `String msg;
      "exception.stacktrace", `String (Printexc.raw_backtrace_to_string bt);
    ]

open struct
  let with_span_real_ ~level ?parent ?data ?__FUNCTION__ ~__FILE__ ~__LINE__
      name f =
    let span =
      let parent =
        match parent with
        | Some _ as p -> p
        | None -> LS.get_in_local_hmap_opt k_parent_scope
      in
      match parent with
      | None ->
        Trace.enter_manual_toplevel_span ~flavor:`Async ?data ~level
          ?__FUNCTION__ ~__FILE__ ~__LINE__ name
      | Some parent ->
        Trace.enter_manual_sub_span ~parent ~flavor:`Async ?data ~level
          ?__FUNCTION__ ~__FILE__ ~__LINE__ name
    in

    (* set current span as parent, for children *)
    LS.set_in_local_hmap k_parent_scope span;

    (* cleanup *)
    let finally () =
      (* restore previous parent span *)
      (match parent with
      | None -> LS.remove_in_local_hmap k_parent_scope
      | Some p -> LS.set_in_local_hmap k_parent_scope p);
      Trace.exit_manual_span span
    in

    try
      let x = f span in
      finally ();
      x
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      add_exn_to_span span e bt;
      Printexc.raise_with_backtrace e bt
end

(** Wrap [f()] in a async span. *)
let with_span ?(level = Trace.get_default_level ()) ?parent ?data ?__FUNCTION__
    ~__FILE__ ~__LINE__ name (f : Trace.explicit_span -> 'a) : 'a =
  if Trace.enabled () && level <= Trace.get_current_level () then
    with_span_real_ ~level ?parent ?data ?__FUNCTION__ ~__FILE__ ~__LINE__ name
      f
  else
    f Trace.Collector.dummy_explicit_span

open struct
  let cons_assoc_opt_ name x l =
    match x with
    | None -> l
    | Some v -> (name, `String v) :: l
end

let enrich_span_process (span : Trace.explicit_span) : unit =
  Trace.add_data_to_manual_span span
    [
      "process.runtime.name", `String "ocaml";
      "process.runtime.version", `String Sys.ocaml_version;
    ]

let enrich_span_service ~name ?namespace ?instance_id ?version
    (span : Trace.explicit_span) : unit =
  let data =
    [ "service.name", `String name ]
    |> cons_assoc_opt_ "service.namespace" namespace
    |> cons_assoc_opt_ "service.instance.id" instance_id
    |> cons_assoc_opt_ "service.version" version
  in
  Trace.add_data_to_manual_span span data

let enrich_span_deployment ?id ?name ~deployment (span : Trace.explicit_span) :
    unit =
  let data =
    [ "deployment.environment.name", `String deployment ]
    |> cons_assoc_opt_ "deployment.id" id
    |> cons_assoc_opt_ "deployment.name" name
  in
  Trace.add_data_to_manual_span span data

module Attributes = struct
  module HTTP = struct
    let error_type = "error.type"
    let request_method = "http.request.method"
    let route = "http.route"
    let url_full = "url.full"

    (** HTTP status code, int *)
    let response_status_code = "http.response.status_code"

    let server_address = "server.address"
    let server_port = "server.port"

    (** http or https *)
    let url_scheme = "url.scheme"
  end

  (** https://github.com/open-telemetry/semantic-conventions/blob/main/docs/resource/host.md *)
  module Host = struct
    let id = "host.id"
    let name = "host.name"
    let type_ = "host.type"
    let arch = "host.arch"
    let ip = "host.ip"
    let mac = "host.mac"
    let image_id = "host.image.id"
    let image_name = "host.image.name"
    let image_version = "host.image.version"
  end
end
