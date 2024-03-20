module SQ = Sync_queue

type t = Executor.t

let pp out _self = Fmt.string out "<background thread>"

let start ?(active = Switch.create ()) ?on_exn ~name () : t =
  let size_name_ = spf "%s.queue-size" name in
  let around_task =
    if Trace.enabled () then (
      let before = ignore in
      let after self () =
        Trace.counter_int size_name_ (Moonpool.Runner.num_tasks self)
      in
      Some (before, after)
    ) else
      None
  in
  let self = Moonpool.Background_thread.create ?around_task ?on_exn ~name () in
  Switch.on_turn_off active (fun () ->
      Moonpool.Runner.shutdown_without_waiting self);
  self

let join = Moonpool.Runner.shutdown

let with_ ?active ?on_exn ~name () f =
  let self = start ?active ?on_exn ~name () in
  let@ () = Fun.protect ~finally:(fun () -> join self) in
  f self
