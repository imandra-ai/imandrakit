type 'a cb = 'a -> unit

type handle = int

type 'a t = {
  gen: int Atomic.t;
  map: 'a cb Int_map.t Atomic.t;
}

let create () : _ t = { gen = Atomic.make 0; map = Atomic.make Int_map.empty }

let[@inline] n_subscribers self : int = Int_map.cardinal (Atomic.get self.map)

let[@inline] has_subscribers self : bool =
  not (Int_map.is_empty (Atomic.get self.map))

let subscribe (self : _ t) cb : handle =
  let id = Atomic.fetch_and_add self.gen 1 in

  while
    let old = Atomic.get self.map in
    let new_m = Int_map.add id cb old in
    not (Atomic.compare_and_set self.map old new_m)
  do
    ()
  done;
  id

let subscribe' self cb : unit = ignore (subscribe self cb : handle)

let unsubscribe (self : _ t) (h : handle) : unit =
  while
    let old = Atomic.get self.map in
    let new_m = Int_map.remove h old in
    not (Atomic.compare_and_set self.map old new_m)
  do
    ()
  done

exception Unsubscribe

let emit self x : unit =
  let m = Atomic.get self.map in
  Int_map.iter
    (fun handle cb -> try cb x with Unsubscribe -> unsubscribe self handle)
    m

let[@inline] forward o1 o2 : unit = subscribe' o1 (emit o2)

let with_subscribe self cb f =
  let h = subscribe self cb in
  Fun.protect f ~finally:(fun () -> unsubscribe self h)
