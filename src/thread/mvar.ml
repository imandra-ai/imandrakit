type 'a t = {
  mutable x: 'a option;
  m: Mutex.t;
  cond: Condition.t;
}

let create_empty () : _ =
  { x = None; m = Mutex.create (); cond = Condition.create () }

let create_full x : _ =
  let self = create_empty () in
  self.x <- Some x;
  self

let[@inline] with_lock self f =
  Mutex.lock self.m;
  Fun.protect ~finally:(fun () -> Mutex.unlock self.m) f

let[@inline] clear self =
  let@ () = with_lock self in
  self.x <- None

let take_block self =
  let@ () = with_lock self in

  let rec loop () =
    match self.x with
    | Some x ->
      self.x <- None;
      x
    | None ->
      Condition.wait self.cond self.m;
      loop ()
  in
  loop ()

let try_take self =
  let@ () = with_lock self in
  match self.x with
  | Some x ->
    self.x <- None;
    (* broadcast, some waiters might be potential take-ers so we need to make
       sure we wake up a put-er if there's any *)
    Condition.broadcast self.cond;
    Some x
  | None -> None

let[@inline] peek self =
  let@ () = with_lock self in
  self.x

let put_block self x =
  let@ () = with_lock self in

  let rec loop () =
    match self.x with
    | Some _ ->
      Condition.wait self.cond self.m;
      loop ()
    | None ->
      self.x <- Some x;
      (* broadcast, some waiters might be potential put-ers so we need to make
         sure we wake up a take-er if there's any *)
      Condition.broadcast self.cond
  in
  loop ()

let try_put self x : bool =
  let@ () = with_lock self in
  match self.x with
  | Some _ -> false
  | None ->
    self.x <- Some x;
    true

let update_block self f =
  let x = take_block self in
  let x', res = f x in
  put_block self x';
  res

let pp ppx out self =
  match peek self with
  | None -> Fmt.string out "<mvar (empty)>"
  | Some x -> Fmt.fprintf out "<@[mvar@ :contains %a@]>" ppx x
