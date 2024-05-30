module A = Atomic

type 'a t = 'a A.t

let rec modify (self : 'a t) f : 'a * 'a =
  let old = A.get self in
  let new_ = f old in
  if A.compare_and_set self old new_ then
    old, new_
  else
    modify self f

let rec modify_with (self : 'a t) f =
  let old = A.get self in
  let x, new_ = f old in
  if A.compare_and_set self old new_ then
    x, old, new_
  else
    modify_with self f
