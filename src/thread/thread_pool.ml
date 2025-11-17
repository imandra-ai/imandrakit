module Runner = Moonpool.Runner

let default_size () : int = max 1 (min 32 (Domain_.recommended_domain_count ()))

type t = Executor.t

let start ?(active = Switch.create ()) ?(on_exn = Executor.default_exn_handler)
    ?(kind = `WorkStealing) ~name ~j () : t =
  let on_init_thread ~dom_id:_ ~t_id () =
    let name_thread = spf "%s.%d" name t_id in
    Trace.set_thread_name name_thread
    (* ignore (Thread.sigmask Unix.SIG_BLOCK [ Sys.sigint; Sys.sigpipe ] : _ list) *)
  in

  let pool =
    match kind with
    | `WorkStealing ->
      Moonpool.Ws_pool.create () ~on_exn ~num_threads:j ~on_init_thread
    | `Fifo ->
      Moonpool.Fifo_pool.create () ~on_exn ~num_threads:j ~on_init_thread
  in

  (* metrics *)
  if Trace.enabled () then (
    let size_name_ = spf "%s.num-tasks" name in
    let gauge = Imandrakit_metrics.Gauge.create_int size_name_ in
    Imandrakit_metrics.add_on_refresh (fun () ->
        Imandrakit_metrics.Gauge.set gauge (Runner.num_tasks pool))
  );

  Switch.on_turn_off active (fun () -> Runner.shutdown_without_waiting pool);
  pool

let with_ ?active ?on_exn ?kind ~name ~j () f =
  let pool = start ?active ?on_exn ?kind ~name ~j () in
  let@ () = Fun.protect ~finally:(fun () -> Runner.shutdown pool) in
  f pool
