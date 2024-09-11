type t = {
  mutex: Mutex.t;
  cond: Condition.t;
}

let create () : t = { mutex = Mutex.create (); cond = Condition.create () }

let wait_block (self : t) : unit =
  Mutex.lock self.mutex;
  Condition.wait self.cond self.mutex;
  Mutex.unlock self.mutex

let unblock (self : t) =
  Mutex.lock self.mutex;
  Condition.broadcast self.cond;
  Mutex.unlock self.mutex
