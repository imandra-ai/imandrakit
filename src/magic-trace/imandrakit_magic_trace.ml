module A = Atomic

external stop_indicator_ : int -> int -> unit = "magic_trace_stop_indicator"
  [@@noalloc]
(** Symbol used to tell magic-trace to take a snapshot *)

let[@inline] now_ts () : int = Ocaml_intrinsics.Perfmon.rdtsc () |> Int64.to_int

(** Start of snapshot *)
let start : int A.t = A.make 0

let mark_start () =
  let now = now_ts () in
  while
    let old = A.get start in
    not (A.compare_and_set start old now)
  do
    ()
  done

let[@inline] trigger_snapshot () = stop_indicator_ (A.get start) 3

let with_snapshot f =
  mark_start ();
  try
    let x = f () in
    trigger_snapshot ();
    x
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    trigger_snapshot ();
    Printexc.raise_with_backtrace e bt
