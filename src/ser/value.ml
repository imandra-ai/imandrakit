open Common_

type t =
  | Null
  | Bool of bool
  | Str of string
  | Bytes of string
  | Int of int64
  | Float of float
  | List of t list
  | Dict of t Str_map.t
  | Tag of int * t

let null = Null
let[@inline] bool b : t = Bool b
let[@inline] int64 i : t = Int i
let[@inline] int i : t = int64 @@ Int64.of_int i
let[@inline] float f : t = Float f
let[@inline] string x : t = Str x
let[@inline] bytes x : t = Bytes x
let[@inline] list x : t = List x
let[@inline] dict x : t = Dict x
let[@inline] dict_of_list l = dict (Str_map.of_list l)
let[@inline] tag i t : t = Tag (i, t)

let rec equal (a : t) (b : t) =
  match a, b with
  | Null, Null -> true
  | Bool x, Bool y -> x = y
  | Int x, Int y -> x = y
  | Float x, Float y -> x = y
  | Bytes x, Bytes y -> x = y
  | Str x, Str y -> x = y
  | List x, List y -> List.length x = List.length y && List.for_all2 equal x y
  | Dict x, Dict y -> Str_map.equal equal x y
  | Tag (x, u), Tag (y, v) -> x = y && equal u v
  | ( ( Null | Bool _ | Int _ | Float _ | Str _ | Bytes _ | List _ | Dict _
      | Tag _ ),
      _ ) ->
    false

let is_null = function
  | Null -> true
  | _ -> false

let rec hash_rec : t -> int =
  let module H = CCHash in
  function
  | Null -> H.int 1
  | Bool b -> H.(combine2 2 (bool b))
  | Int i -> H.(combine2 3 (int64 i))
  | Str s -> H.(combine2 4 (string s))
  | Bytes s -> H.(combine2 5 (string s))
  | Float f -> H.(combine2 6 (int64 (Int64.bits_of_float f)))
  | List l -> H.(combine2 7 (list hash_rec l))
  | Dict m -> H.(combine2 8 (iter (pair string hash_rec) (Str_map.to_iter m)))
  | Tag (i, t) -> H.(combine3 9 (int i) (hash_rec t))

let hash = hash_rec

let rec pp out (self : t) =
  match self with
  | Null -> Fmt.string out "null"
  | Bool b -> Fmt.bool out b
  | Int i -> Fmt.int64 out i
  | Str s -> Fmt.Dump.string out s
  | Bytes s -> Fmt.fprintf out "(bytes %S)" s
  | Float f -> Fmt.fprintf out "%f" f
  | List l -> Fmt.Dump.list pp out l
  | Dict m ->
    Fmt.fprintf out "{@[%a@]}"
      (pp_iter ~sep:", " Fmt.Dump.(pair string pp))
      (Str_map.to_iter m)
  | Tag (i, t) -> Fmt.fprintf out "%d(@[%a@])" i pp t

let show = Fmt.to_string pp
