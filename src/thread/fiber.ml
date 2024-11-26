include Moonpool_fib
module FLS = Moonpool_fib.Fls

(** Access the context (inheritable hmap) *)
let k_rcontext : Hmap.t FLS.t = FLS.k_local_hmap

(** Access the current rcontext *)
let[@inline] get_rcontext () : Hmap.t = FLS.get ~default:Hmap.empty k_rcontext

let[@inline] get_from_rcontext (k : 'a Hmap.key) : 'a option =
  FLS.get_in_local_hmap_opt k

(** Add some k/v to the context *)
let[@inline] add_to_rcontext (k : 'a Hmap.key) (x : 'a) : unit =
  FLS.set_in_local_hmap k x

let add_on_cancel' fib k : unit = ignore (add_on_cancel fib k : cancel_handle)
let add_on_cancel_any' (Any fib) k : unit = add_on_cancel' fib k

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
