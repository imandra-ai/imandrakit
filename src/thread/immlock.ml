module A = Atomic

type 'a t = {
  mutex: Mutex.t;  (** Mutex is only used for updates *)
  content: 'a A.t;  (** Quick access immutable data *)
}

let create content = { mutex = Mutex.create (); content = A.make content }

let with_lock l f =
  Mutex.lock l.mutex;
  try
    let x = f l.content in
    Mutex.unlock l.mutex;
    x
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    Mutex.unlock l.mutex;
    Printexc.raise_with_backtrace e bt

let update self f =
  with_lock self (fun a ->
      let res = f (Atomic.get a) in
      A.set self.content res)

let update_map self f =
  with_lock self (fun a ->
      let res, y = f (A.get a) in
      A.set self.content res;
      y)

let[@inline] get l = A.get l.content

let pp ppx out self =
  let x = get self in
  Fmt.fprintf out "(@[immlock@ %a@])" ppx x
