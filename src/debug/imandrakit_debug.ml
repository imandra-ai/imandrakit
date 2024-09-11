module Event = Imandrakit_debug_core.Event
module Debug = Debug
module Subscribers = Subscribers
module Server = Server

(** Suspend current thread, wait for debugger *)
let suspend () =
  Logs.debug (fun k ->
      k "debug: suspending thread %d" (Thread.id @@ Thread.self ()));
  Debug.Private_.block_current_thread ()

let setup = Server.setup
