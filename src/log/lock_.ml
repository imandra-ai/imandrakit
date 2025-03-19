open struct
  external reraise : exn -> 'a = "%reraise"
end

type 'a t = {
  v: 'a;
  m: Mutex.t;
}

let create v = { v; m = Mutex.create () }

let with_ (self : _ t) f =
  Mutex.lock self.m;
  match f self.v with
  | x ->
    Mutex.unlock self.m;
    x
  | exception e ->
    Mutex.unlock self.m;
    reraise e
