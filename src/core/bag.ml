type 'a t =
  | Em
  | Ret of 'a
  | App of 'a t * 'a t

let empty = Em

let[@inline] is_empty = function
  | Em -> true
  | _ -> false

let[@inline] return x = Ret x

let append x y =
  match x, y with
  | Em, _ -> y
  | _, Em -> x
  | _ -> App (x, y)

let[@inline] cons x self = append (return x) self
let[@inline] snoc self x = append self (return x)

let rec of_list = function
  | [] -> empty
  | x :: tl -> cons x (of_list tl)

let of_iter i = Iter.fold snoc empty i
let append_l a l = append a (of_list l)

let map_opt f = function
  | None -> empty
  | Some x -> f x

let rec map f = function
  | Em -> Em
  | Ret x -> Ret (f x)
  | App (l1, l2) -> App (map f l1, map f l2)

let rec flat_map f = function
  | Em -> Em
  | Ret x -> f x
  | App (l1, l2) -> append (flat_map f l1) (flat_map f l2)

let rec filter f = function
  | Em -> Em
  | Ret x ->
    if f x then
      Ret x
    else
      Em
  | App (l1, l2) -> append (filter f l1) (filter f l2)

let rec filter_map f = function
  | Em -> Em
  | Ret x ->
    (match f x with
    | None -> Em
    | Some y -> Ret y)
  | App (l1, l2) -> append (filter_map f l1) (filter_map f l2)

let rec flat_map_l f l =
  match l with
  | [] -> empty
  | [ x ] -> f x
  | x :: tl -> append (f x) (flat_map_l f tl)

let iter f b : unit =
  let rec aux = function
    | Em -> ()
    | Ret x -> f x
    | App (x, y) ->
      aux x;
      aux y
  in
  aux b

let to_iter self k = iter k self

let fold_left f acc b =
  let rec aux acc = function
    | Em -> acc
    | Ret x -> f acc x
    | App (x, y) ->
      let acc = aux acc x in
      aux acc y
  in
  aux acc b

let fold_right f b acc =
  let rec aux acc = function
    | Em -> acc
    | Ret x -> f x acc
    | App (x, y) ->
      let acc = aux acc y in
      aux acc x
  in
  aux acc b

let length self = fold_left (fun n _ -> n + 1) 0 self
let to_list b = fold_right List.cons b []

module Infix = struct
  let ( @ ) = append
end

include Infix
