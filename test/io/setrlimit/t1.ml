module R = Imandrakit_io_setrlimit

let () =
  R.set_exn R.RLIMIT_CPU 500;
  ()
