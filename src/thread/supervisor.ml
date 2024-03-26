module Log = (val Logger.mk_log_str "imandrakit.supervisor")

let retry_loop (type a) ?(max = 10) ?(initial_delay_before_restart_s = 0.001)
    ?(max_delay_before_restart_s = 60.) f : a =
  let exception Ret of a in
  let delay = ref (Stdlib.max 0.000_001 initial_delay_before_restart_s) in
  let count = ref 0 in

  try
    while true do
      match f () with
      | res -> raise_notrace (Ret res)
      | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        let err = Error.of_exn ~bt ~kind:Error_kind.generic_internal_error e in
        if !count < max then
          Log.err (fun k ->
              k "task failed (retry %d/%d):@ %a" !count max Error.pp err)
        else
          Error.raise_err err;
        Thread.delay !delay;
        delay := min (!delay *. 2.) max_delay_before_restart_s
    done;
    assert false
  with Ret x -> x
