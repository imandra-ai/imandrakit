type 'a t = 'a Atomic.t

val modify : 'a t -> ('a -> 'a) -> 'a * 'a
(** [modify a f] is [a := f !a], returns the old and new values *)

val modify_with : 'a t -> ('a -> 'b * 'a) -> 'b * 'a * 'a
(** [modify_with a f] is like
    [let old = !a in let x, y = f old in a := y; x,old,y]. Returns the old
    value, new value, and a side value of type ['b]. *)
