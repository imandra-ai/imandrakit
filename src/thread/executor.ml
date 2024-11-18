(** A task executor. *)

type t = Moonpool.Runner.t

let size = Moonpool.Runner.size
let pp out _ = Fmt.string out "<executor>"
let run = Moonpool.Runner.run_async
let shutdown = Moonpool.Runner.shutdown
let shutdown_without_waiting = Moonpool.Runner.shutdown_without_waiting

(** Default exception handler, logs the error *)
let default_exn_handler exn bt =
  Logs.err (fun k ->
      k "Uncaught error in executor:@ %s@ backtrace:@.%s"
        (Printexc.to_string exn)
        (Printexc.raw_backtrace_to_string bt))
