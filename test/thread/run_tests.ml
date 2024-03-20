let suite = [ T_timer.get (); T_fut.get (); T_rvar.get () ]

let () = Testlib.run_all ~descr:"imandrax.util.thread tests" suite
