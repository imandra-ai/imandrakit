module A = Atomic

type 'a cb = 'a option -> unit

type 'a on_change =
  | St_frozen
  | St_active of 'a cb list

type 'a st = {
  v: 'a;
  on_change: 'a on_change;
      (** [on_change] is either [None] (no more changes) or [Some fs] (subscribers) *)
  _keepalive: Obj.t list;  (** Objects to prevent from being collected. *)
}

type 'a t = { st: 'a st Lock.t } [@@unboxed]

let[@inline] get (self : 'a t) : 'a = (Lock.get self.st).v

let[@inline] pp ppx out self : unit = ppx out (get self)

let[@inline] return x =
  { st = Lock.create { v = x; on_change = St_frozen; _keepalive = [] } }

let mk_var_ _keepalive x =
  { st = Lock.create { v = x; on_change = St_active []; _keepalive } }

let[@inline] mk_var x = mk_var_ [] x

exception Frozen

let _on_uncaught =
  ref (fun exn ->
      Printf.eprintf
        "Error: uncaught exception in Rvar.on_change handler:\n```\n%s\n```\n%!"
        (Printexc.to_string exn))

let freeze (type a) (self : a t) : unit =
  let cbs =
    let@ st_ref = Lock.with_lock_as_ref self.st in
    match Lock.LockRef.get st_ref with
    | { on_change = St_frozen; _ } -> None
    | { on_change = St_active fs; _ } as old ->
      let new_ = { old with on_change = St_frozen } in
      Lock.LockRef.set st_ref new_;
      Some fs
  in
  match cbs with
  | None -> ()
  | Some fs ->
    (* tell listener we're done changing *)
    List.iter (fun f -> try f None with e -> !_on_uncaught e) fs

let set (type a) (self : a t) x =
  let fs =
    let@ st_ref = Lock.with_lock_as_ref self.st in
    match Lock.LockRef.get st_ref with
    | { on_change = St_frozen; _ } -> raise Frozen
    | { on_change = St_active fs; _ } as old ->
      let new_ = { old with v = x } in
      Lock.LockRef.set st_ref new_;
      fs
  in
  (* notify listeners of the change *)
  List.iter (fun f -> try f (Some x) with e -> !_on_uncaught e) fs

let update (self : 'a t) ~f : _ =
  let v', ret, fs =
    let@ st_ref = Lock.with_lock_as_ref self.st in
    match Lock.LockRef.get st_ref with
    | { on_change = St_frozen; _ } -> raise Frozen
    | { on_change = St_active fs; _ } as old ->
      let v', ret = f old.v in
      let new_ = { old with v = v' } in
      Lock.LockRef.set st_ref new_;
      v', ret, fs
  in
  (* notify listeners of the change *)
  List.iter (fun f -> try f (Some v') with e -> !_on_uncaught e) fs;
  ret

let on_change self (f : _ cb) : unit =
  let frozen =
    let@ st_ref = Lock.with_lock_as_ref self.st in
    match Lock.LockRef.get st_ref with
    | { on_change = St_frozen; _ } -> true
    | { on_change = St_active fs; _ } as old ->
      let new_ = { old with on_change = St_active (f :: fs) } in
      Lock.LockRef.set st_ref new_;
      false
  in

  (* if [self] is frozen, call [f] now to ensure cleanup. *)
  if frozen then f None

let of_fut fut : _ t =
  match Fut.peek fut with
  | Some (Ok x) -> return (Some x)
  | Some (Error _) -> return None
  | None ->
    let v = mk_var_ [ Obj.repr fut ] None in
    Fut.on_result fut (function
      | Ok x ->
        set v (Some x);
        freeze v
      | Error _ -> freeze v);
    v

let of_fut_or_error fut : _ t =
  match Fut.peek fut with
  | Some r -> return (Some r)
  | None ->
    let v = mk_var_ [ Obj.repr fut ] None in
    Fut.on_result fut (fun r ->
        set v (Some r);
        freeze v);
    v

let map ~f (rv : _ t) : _ t =
  let res = mk_var_ [ Obj.repr rv ] (f (get rv)) in
  on_change rv (function
    | None -> freeze res
    | Some x -> set res (f x));
  res

let map2 ~f v1 v2 : _ t =
  let res = mk_var_ [ Obj.repr v1; Obj.repr v2 ] (f (get v1) (get v2)) in
  let n_alive = A.make 2 in
  let on_input_frozen () =
    if A.fetch_and_add n_alive (-1) = 1 then freeze res
  in
  on_change v1 (function
    | None -> on_input_frozen ()
    | Some x -> set res (f x (get v2)));
  on_change v2 (function
    | None -> on_input_frozen ()
    | Some y -> set res (f (get v1) y));
  res

let flatten (v : 'a t t) : 'a t =
  let res = mk_var_ [ Obj.repr v ] (get (get v)) in

  let top_frozen = Atomic.make false in
  let cur_num = Atomic.make 0 in

  (* subscribe to future changes in a value of [v] *)
  let handle_sub_change ~cur sub_v =
    on_change sub_v (function
      | None ->
        (* freeze output if the last input is frozen *)
        if Atomic.get top_frozen && cur = Atomic.get cur_num then freeze res
      | Some x ->
        (* still the current sub_v? *)
        if cur = Atomic.get cur_num then set res x)
  in

  handle_sub_change ~cur:0 (get v);

  on_change v (function
    | None -> Atomic.set top_frozen true
    | Some sub_v ->
      (* now subscribe to [sub_v].
          NOTE: potential resource leak if the previous value of [v] still exists
         and continues emitting changes. *)
      let cur = Atomic.fetch_and_add cur_num 1 + 1 in

      (* first, change to current value*)
      set res (get sub_v);
      handle_sub_change ~cur sub_v);
  res

let flat_map ~f v = map ~f v |> flatten

let pick (l : _ t list) : _ t =
  match l with
  | [] -> invalid_arg "Rvar.merge: empty list"
  | [ x ] -> x
  | x :: _ as l ->
    let res = mk_var_ (Obj.magic l : Obj.t list) (get x) in
    let n_alive = A.make (List.length l) in

    let on_input_changed_ = function
      | None -> if A.fetch_and_add n_alive (-1) = 1 then freeze res
      | Some x -> set res x
    in

    List.iter (fun v -> on_change v on_input_changed_) l;
    res

let cutoff ?(eq = Stdlib.( = )) v : _ t =
  let res = mk_var_ [ Obj.repr v ] (get v) in
  on_change v (function
    | None -> freeze res
    | Some x -> if not (eq x (get res)) then set res x);
  res

module Monoid = struct
  type 'a t = 'a * ('a -> 'a -> 'a)

  let empty = fst

  let merge = snd
end

let monoid_merge (m : 'm Monoid.t) ~f v1 v2 : 'm t =
  map2 v1 v2 ~f:(fun x y -> Monoid.merge m (f x) (f y))

let monoid_merge_a (m : 'm Monoid.t) ~f (a : _ t array) : 'm t =
  match Array.length a with
  | 0 -> return (Monoid.empty m)
  | 1 -> map a.(0) ~f
  | _ ->
    (* build a tree of merges *)
    let rec build_tree i j =
      if i = j then
        map a.(i) ~f
      else if i + 1 = j then
        monoid_merge m ~f a.(i) a.(j)
      else (
        let mid = (i / 2) + (j / 2) in
        let lower_half = build_tree i mid in
        let upper_half = build_tree (mid + 1) j in
        map2 lower_half upper_half ~f:(Monoid.merge m)
      )
    in
    build_tree 0 (Array.length a - 1)

let monoid_merge_l (m : 'm Monoid.t) ~f (l : _ t list) : 'm t =
  match l with
  | [] -> return (Monoid.empty m)
  | [ x ] -> map x ~f
  | l ->
    let a = Array.of_list l in
    monoid_merge_a m ~f a

module Infix = struct
  let[@inline] ( let+ ) x f = map ~f x

  let[@inline] ( let* ) x f = flat_map ~f x

  let[@inline] ( and+ ) a b = map2 a b ~f:(fun x y -> x, y)

  let ( and* ) = ( and+ )
end

include Infix
