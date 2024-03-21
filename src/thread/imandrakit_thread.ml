module Background_thread = Background_thread
module Executor = Executor
module Fiber = Fiber
module Fut = Fut
module Immlock = Immlock
module LRU_cache = LRU_cache
module Lock = Lock
module Mvar = Mvar
module Once = Once
module Rvar = Rvar
module Switch = Switch
module Sync_queue = Sync_queue
module Thread_pool = Thread_pool
module Timer = Timer

(** {2 Re-export} *)

module Exn_bt = Moonpool.Exn_bt

let[@inline] with_lock (m : Mutex.t) f =
  Mutex.lock m;
  Fun.protect ~finally:(fun () -> Mutex.unlock m) f
