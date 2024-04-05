module A = Atomic

type kind =
  | Counter
  | Gauge
[@@deriving show { with_path = false }]

type 'a m = {
  kind: kind;
  name: string;
  v: 'a A.t;
}
(* FIXME: this will not be needed once we have prometheus export *)
[@@ocaml.warning "-69"]

type state = {
  ints: int m list;
  floats: float m list;
  updates: (unit -> unit) list;
  gc_metrics: bool;
}

let empty : state = { ints = []; floats = []; updates = []; gc_metrics = false }
let global : state Immlock.t = Immlock.create empty

let add_int_ c =
  Immlock.update global (fun st -> { st with ints = c :: st.ints })

let add_float_ c =
  Immlock.update global (fun st -> { st with floats = c :: st.floats })

let add_on_update_ c =
  Immlock.update global (fun st -> { st with updates = c :: st.updates })

module M_ = struct
  type 'a t = 'a m

  let create_int kind name : _ t =
    let c = { kind; name; v = A.make 0 } in
    add_int_ c;
    c

  let create_float kind name : _ t =
    let c = { kind; name; v = A.make 0. } in
    add_float_ c;
    c

  let[@inline] set self v = A.set self.v v
end

module Counter = struct
  include M_

  let create_int = create_int Counter
  let create_float = create_float Counter
  let[@inline] incr self = A.incr self.v

  let[@inline] incr_by self n =
    assert (n >= 0);
    ignore (A.fetch_and_add self.v n : int)
end

module Gauge = struct
  include M_

  let create_int = create_int Gauge
  let create_float = create_float Gauge
end

let add_on_refresh = add_on_update_

let add_gc_metrics () : unit =
  let must_add =
    Immlock.update_map global (fun st ->
        if st.gc_metrics then
          st, false
        else
          { st with gc_metrics = true }, true)
  in

  if must_add then (
    let c_minor_collections =
      Counter.create_int "process.runtime.ocaml.gc.minor_collections"
    in
    let c_major_collections =
      Counter.create_int "process.runtime.ocaml.gc.major_collections"
    in
    let c_major_heap_size =
      Gauge.create_float "process.runtime.ocaml.gc.major_heap"
    in

    add_on_refresh (fun () ->
        let stat = Gc.quick_stat () in

        Counter.set c_minor_collections stat.minor_collections;
        Counter.set c_major_collections stat.major_collections;
        Counter.set c_major_heap_size (float (8 * stat.heap_words)))
  )

let iter_all ~int ~float () =
  let st = Immlock.get global in
  List.iter (fun f -> f ()) st.updates;
  List.iter (fun m -> int m.kind m.name @@ A.get m.v) st.ints;
  List.iter (fun m -> float m.kind m.name @@ A.get m.v) st.floats

let emit_trace_ () =
  iter_all
    ~int:(fun _k name v -> Trace.counter_int name v)
    ~float:(fun _k name v -> Trace.counter_float name v)
    ()

let emit_trace () = if Trace.enabled () then emit_trace_ ()

let emit_trace_every ~period_s (timer : Timer.t) : unit =
  if Trace.enabled () then Timer.run_every_s timer period_s emit_trace_
