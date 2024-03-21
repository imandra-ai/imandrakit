(** Blocking queue *)

include Moonpool.Blocking_queue
(** @inline *)

let pp out (self : _ t) : unit =
  Fmt.fprintf out "<sync queue (%d elts)>" (size self)

(** Iterate on items. *)
let to_iter self : _ Iter.t =
 fun yield ->
  try
    while true do
      let x = pop self in
      yield x
    done
  with Closed -> ()
