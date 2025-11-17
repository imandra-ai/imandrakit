type t = Executor.t

let pp out _self = Fmt.string out "<background thread>"

let start ?(active = Switch.create ()) ?on_exn ~name () : t =
  let self = Moonpool.Background_thread.create ?on_exn ~name () in

  (* metrics *)
  if Trace.enabled () then (
    let size_name_ = spf "%s.queue-size" name in
    let gauge = Imandrakit_metrics.Gauge.create_int size_name_ in
    Imandrakit_metrics.add_on_refresh (fun () ->
        Imandrakit_metrics.Gauge.set gauge (Moonpool.Runner.num_tasks self))
  );

  Switch.on_turn_off active (fun () ->
      Moonpool.Runner.shutdown_without_waiting self);
  self

let join = Moonpool.Runner.shutdown

let with_ ?active ?on_exn ~name () f =
  let self = start ?active ?on_exn ~name () in
  let@ () = Fun.protect ~finally:(fun () -> join self) in
  f self
