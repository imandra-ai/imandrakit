module Log = (val Logger.mk_log_str "x.supervisor")

type attempt_number = int

let retry_loop (type a) ?(max = 10) ?(initial_delay_before_restart_s = 0.001)
    ?(max_delay_before_restart_s = 60.) () f : a =
  let exception Ret of a in
  let delay = ref (Stdlib.max 0.001 initial_delay_before_restart_s) in
  let count = ref 0 in

  let rd_st = lazy (Random.State.make_self_init ()) in
  (* avoid thundering herd *)
  let jitter_ms (delay : float) =
    Random.State.float (Lazy.force rd_st) (delay /. 5.)
  in

  try
    while true do
      incr count;
      match f !count with
      | res -> raise_notrace (Ret res)
      | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        let err = Error.of_exn ~bt ~kind:Error_kind.generic_internal_error e in

        if !count <= max then
          Log.err (fun k ->
              k "task failed (attempt %d/%d):@ %a" !count max Error.pp err)
        else
          (* exit loop with error *)
          Error.raise_err err;

        (* sleep before retrying *)
        let actual_delay = !delay +. jitter_ms !delay in
        (let@ _sp =
           Trace.with_span ~__FILE__ ~__LINE__ "supervisor.retry-loop.sleep"
             ~data:(fun () -> [ "delay_s", `Float actual_delay ])
         in
         Thread.delay actual_delay);

        delay := min (!delay *. 2.) max_delay_before_restart_s
    done;
    assert false
  with Ret x -> x
