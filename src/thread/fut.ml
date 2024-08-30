include Moonpool.Fut

type backtrace = Printexc.raw_backtrace
type exn_with_bt = exn * Printexc.raw_backtrace

let raise_with_bt e bt = Printexc.raise_with_backtrace e bt

let pp ppx out (self : _ t) : unit =
  match peek self with
  | None -> Fmt.fprintf out "<unresolved future>"
  | Some (Ok x) -> Fmt.fprintf out "<@[future res=%a@]>" ppx x
  | Some (Error ebt) ->
    Fmt.fprintf out "<@[future err=%S@]>" (Printexc.to_string ebt.exn)

let[@inline] unwrap = function
  | Ok x -> x
  | Error (e, bt) -> raise_with_bt e bt

let peek_exn self : _ option =
  match peek self with
  | Some (Ok x) -> Some x
  | Some (Error ebt) -> Exn_bt.raise ebt
  | None -> None

let map_iter ~f (it : _ Iter.t) : _ Iter.t t =
  let open! Infix in
  let+ arr = join_array (Iter.map f it |> Iter.to_array) in
  Iter.of_array arr

let[@inline] map_l ~f l : _ list t = CCList.map f l |> join_list

let protect ~finally f : _ t =
  let fut, promise = make () in
  let sub = f () in
  on_result sub (fun res ->
      let cleanup = finally () in
      on_result cleanup (fun _ -> fulfill_idempotent promise res));
  fut

let wait_vec (vec : (_ t, _) Vec.t) : unit t =
  Advanced.barrier_on_abstract_container_of_futures ~iter:Vec.iter
    ~len:Vec.length
    ~aggregate_results:(fun _f _ -> ())
    vec

let map_vec_array (vec : ('a t, _) Vec.t) : 'a array t =
  Advanced.barrier_on_abstract_container_of_futures ~iter:Vec.iter
    ~len:Vec.length
    ~aggregate_results:(fun f vec ->
      Array.init (Vec.length vec) (fun i -> f @@ Vec.get vec i))
    vec

type any = Any : _ t -> any

let pp_any out (Any x) = pp Fmt.opaque out x
let[@inline] any_is_resolved (Any x) = is_resolved x
let[@inline] any_is_success (Any x) = is_success x
let[@inline] any_is_failed (Any x) = is_failed x
let[@inline] any_raise_if_failed (Any x) = raise_if_failed x
