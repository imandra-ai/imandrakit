let suite = [ T_timer.get (); T_fut.get (); T_rvar.get () ]
let () = Testlib.run_all ~descr:"imandrakit.thread tests" suite
