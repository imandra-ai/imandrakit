type 'a t = {
  hd: 'a list;
  tl: 'a list;
}
(** Queue containing elements of type 'a.

      invariant: if hd=[], then tl=[] *)

let[@inline] list_is_empty_ = function
  | [] -> true
  | _ :: _ -> false

let[@inline] return x : _ t = { hd = [ x ]; tl = [] }

let[@inline] make_ hd tl =
  match hd with
  | [] -> { hd = List.rev tl; tl = [] }
  | _ :: _ -> { hd; tl }

let empty = { hd = []; tl = [] }
let[@inline] is_empty self = list_is_empty_ self.hd
let[@inline] push self x : _ t = make_ self.hd (x :: self.tl)
let[@inline] length self = List.length self.hd + List.length self.tl
let push_l self l : _ t = make_ self.hd (List.rev_append l self.tl)

(** Iterate on a list in reverse *)
let rev_iter_ f l =
  let rec loop i l =
    match l with
    | [] -> ()
    | _ when i = 0 -> List.iter f (List.rev l)
    | x :: tl ->
      loop (i - 1) tl;
      f x
  in
  loop 1_000 l

let iter f (self : _ t) : unit =
  List.iter f self.hd;
  rev_iter_ f self.tl

let to_iter self k = iter k self

exception Empty

let pop_exn self =
  match self.hd with
  | [] ->
    assert (list_is_empty_ self.tl);
    raise Empty
  | x :: hd' ->
    let self' = make_ hd' self.tl in
    x, self'

let of_list l = make_ [] l

let to_list (self : _ t) =
  if list_is_empty_ self.tl then
    self.hd
  else
    CCList.append self.hd (List.rev self.tl)

let to_rev_list self =
  if list_is_empty_ self.hd then
    self.tl
  else
    CCList.append self.tl (List.rev self.hd)

let append a b = { hd = to_list a; tl = to_rev_list b }

let pp ppx out self : unit =
  Fmt.fprintf out "<@[fqueue@ %a@]>" (Util.pp_iter ppx) (to_iter self)
