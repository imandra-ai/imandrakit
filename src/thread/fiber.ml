include Moonpool_fib
module FLS = Moonpool_fib.Fls

(** Access the context *)
let k_rcontext : Hmap.t FLS.key = FLS.new_key ~init:(fun () -> Hmap.empty) ()

(** Access the current rcontext *)
let[@inline] get_rcontext () : Hmap.t = FLS.get k_rcontext

let[@inline] get_from_rcontext (k : 'a Hmap.key) : 'a option =
  FLS.get_opt k_rcontext |> CCOption.flat_map (Hmap.find k)

(** Add some k/v to the context *)
let add_to_rcontext (k : 'a Hmap.key) (x : 'a) : unit =
  let ctx = get_rcontext () in
  FLS.set k_rcontext (Hmap.add k x ctx)

(** An easy starting point to mimic future-returning APIs *)
let spawn_top_and_return_fut ~on (f : unit -> 'a) : 'a Fut.t =
  spawn_top ~on f |> res

let spawn_and_return_fut ?on (f : unit -> 'a) : 'a Fut.t = spawn ?on f |> res
let spawn_top_and_ignore ~on f : unit = ignore (spawn_top ~on f : _ t)

let both ?protect f1 f2 =
  let f1 = spawn ?protect f1 in
  let f2 = spawn ?protect f2 in
  f1, f2

let both_await ?protect f1 f2 =
  let f1 = spawn ?protect f1 in
  let f2 = spawn ?protect f2 in
  await f1, await f2

let both_await_ignore ?protect f1 f2 : unit = ignore (both_await ?protect f1 f2)

let pp ppx out (self : _ t) =
  match peek self with
  | Some (Ok x) -> Fmt.fprintf out "<@[fiber@ :done %a@]>" ppx x
  | Some (Error _) -> Fmt.string out "<fiber :failed>"
  | None -> Fmt.string out "<fiber :in-progress>"
