module I_map = Int_map

type cb = unit -> unit

type state =
  | On of {
      n: int;
      m: cb Int_map.t;  (** removable callbacks *)
      l: cb list;
    }
  | Off

type t = { st: state Atomic.t } [@@unboxed]

let update_ (type a) (self : t) (f : state -> a * state) : a =
  let rec loop () =
    let old_st = Atomic.get self.st in
    let x, new_st = f old_st in
    if old_st == new_st || Atomic.compare_and_set self.st old_st new_st then
      x
    else
      (loop [@tailcall]) ()
  in
  loop ()

let on_turn_off (self : t) (f : cb) : unit =
  let must_fire =
    update_ self (function
      | Off -> true, Off
      | On r -> false, On { r with l = f :: r.l })
  in
  if must_fire then (* call now *) f ()

let with_on_turn_off (self : t) (cb : cb) f =
  let must_fire, cb_handle =
    update_ self (function
      | Off -> (true, 0), Off
      | On r ->
        (* store [cb] in the local map, with unique ID [r.n] *)
        (false, r.n), On { r with n = r.n + 1; m = Int_map.add r.n cb r.m })
  in

  if must_fire then (
    (* switch is already off, just call [cb] now and tailcall into [f] *)
    cb ();
    f ()
  ) else (
    (* cleanup: remove the callback *)
    let remove_cb () : unit =
      update_ self (function
        | Off -> (), Off
        | On r -> (), On { r with m = Int_map.remove cb_handle r.m })
    in

    Fun.protect f ~finally:remove_cb
  )

let turn_off' ?(trace = true) self =
  (* When calling turn_off' from a signal handler, Trace.message may cause the thread
     to be killed. For this reason, we provide a way to disable tracing here. *)
  if trace then Trace.message "switch.turn-off";
  match Atomic.exchange self.st Off with
  | Off -> `Was_off
  | On { l; m; n = _ } ->
    List.iter (fun f -> f ()) l;
    Int_map.iter (fun _ f -> f ()) m;
    `Was_on

let[@inline] turn_off ?(trace = true) self =
  ignore (turn_off' self ~trace : [> `Was_on ])

let create ?parent () : t =
  let self = { st = Atomic.make (On { l = []; n = 0; m = Int_map.empty }) } in
  Option.iter (fun p -> on_turn_off p (fun () -> turn_off self)) parent;
  self

let[@inline] is_on self : bool =
  match Atomic.get self.st with
  | On _ -> true
  | Off -> false

let[@inline] is_off self = not (is_on self)

let pp out self = Fmt.fprintf out "<switch on=%B>" (is_on self)

let show = Fmt.to_string pp
