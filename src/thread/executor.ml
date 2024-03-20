(** A task executor. *)

type t = Moonpool.Runner.t

let pp out _ = Fmt.string out "<executor>"
let run = Moonpool.Runner.run_async
let shutdown = Moonpool.Runner.shutdown

(** Default exception handler, logs the error *)
let default_exn_handler exn bt =
  Logs.err (fun k ->
      k "Uncaught error in executor:@ %s@ backtrace:@.%s"
        (Printexc.to_string exn)
        (Printexc.raw_backtrace_to_string bt))
