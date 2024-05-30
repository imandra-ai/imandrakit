type 'a state =
  | Done of 'a
  | Wait of (unit -> 'a)

type 'a t = { lock: 'a state Lock.t } [@@unboxed]

let create f : _ t = { lock = Lock.create (Wait f) }

let get (self : 'a t) : 'a =
  Lock.with_lock_as_ref self.lock (fun r ->
      match Lock.LockRef.get r with
      | Done x -> x
      | Wait f ->
        let x = f () in
        Lock.LockRef.set r (Done x);
        x)
