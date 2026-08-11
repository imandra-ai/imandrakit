module R = Imandrakit_io_setrlimit

let () =
  R.set_hard_exn R.RLIMIT_CPU (Some 500n);
  ()
