module R = Imandrakit_io_setrlimit

let () =
  (* 2 MB max *)
  R.set_exn R.RLIMIT_AS 2_000_000;
  (* now allocate a 8MB array *)
  try
    let _arr = Sys.opaque_identity (Array.make 1_000_000 true) in
    ();
    failwith "nothing happened"
  with Out_of_memory ->
    print_endline "memory correctly limited";
    ()
