open struct
  module Fmt = CCFormat
  module V = Value
end

module Error = struct
  type t = {
    msg: string;
    v: V.t;
    subs: t list;
  }

  let mk ?(subs = []) msg v : t = { msg; v; subs }
  let of_string s v : t = mk s v

  let pp out (self : t) =
    let rec pp out self =
      Fmt.fprintf out "@[<v2>@[<2>%s@ in value %a@]" self.msg V.pp self.v;
      List.iter
        (fun s -> Fmt.fprintf out "@ @[<2>sub-error:@ %a@]" pp s)
        self.subs;
      Fmt.fprintf out "@]"
    in
    Fmt.fprintf out "@[<2>Ser_decode.error:@ %a@]" pp self

  let show = Fmt.to_string pp
end

exception Fail of Error.t

type 'a t = { deser: V.t -> 'a } [@@unboxed]

let[@inline] fail_ msg v = raise_notrace (Fail (Error.mk msg v))
let[@inline] fail_e e = raise_notrace (Fail e)
let fail_err e = { deser = (fun _ -> fail_e e) }
let return x = { deser = (fun _ -> x) }
let fail s = { deser = (fun v -> fail_ s v) }
let failf fmt = Fmt.kasprintf fail fmt
let delay f = { deser = (fun v -> (f ()).deser v) }

let return_result = function
  | Ok x -> return x
  | Error s -> fail s

let return_result_err = function
  | Ok x -> return x
  | Error e -> fail_err e

let unwrap_opt msg = function
  | Some x -> return x
  | None -> fail msg

let any = { deser = (fun v -> v) }

let bool =
  {
    deser =
      (function
      | V.Bool b -> b
      | V.Int 1L -> true
      | V.Int 0L -> false
      | v -> fail_ "expected bool" v);
  }

let int64 =
  {
    deser =
      (function
      | V.Int i -> i
      | v -> fail_ "expected int" v);
  }

let int = { deser = (fun v -> Int64.to_int (int64.deser v)) }

let string =
  {
    deser =
      (function
      | V.Str s | V.Bytes s -> s
      | v -> fail_ "expected string" v);
  }

let reflect dec v =
  {
    deser =
      (fun _ ->
        match dec.deser v with
        | x -> Ok x
        | exception Fail e -> Error e);
  }

let apply dec v = { deser = (fun _ -> dec.deser v) }

let list d =
  {
    deser =
      (function
      | V.List l -> List.map (fun x -> d.deser x) l
      | v -> fail_ "expected list" v);
  }

let dict =
  {
    deser =
      (function
      | V.Dict m -> m
      | v -> fail_ "expected dict" v);
  }

let dict_field name d =
  {
    deser =
      (function
      | V.Dict m as v ->
        (match Str_map.find_opt name m with
        | None -> fail_ (Printf.sprintf "did not find key %S" name) v
        | Some x -> d.deser x)
      | v -> fail_ "expected dict" v);
  }

let dict_field_opt name d =
  {
    deser =
      (function
      | V.Dict m ->
        (match Str_map.find_opt name m with
        | None -> None
        | Some x -> Some (d.deser x))
      | v -> fail_ "expected dict" v);
  }

let both a b =
  {
    deser =
      (fun v ->
        let xa = a.deser v in
        let xb = b.deser v in
        xa, xb);
  }

let ( >>= ) d f =
  {
    deser =
      (fun v ->
        let x = d.deser v in
        (f x).deser v);
  }

let ( >|= ) d f =
  {
    deser =
      (fun v ->
        let x = d.deser v in
        f x);
  }

let try_l l =
  {
    deser =
      (fun v ->
        let subs = ref [] in
        match
          CCList.find_map
            (fun d ->
              match d.deser v with
              | x -> Some x
              | exception Fail err ->
                subs := err :: !subs;
                None)
            l
        with
        | Some x -> x
        | None -> fail_e (Error.mk "all decoders failed" v ~subs:!subs));
  }

module Infix = struct
  let ( >>= ) = ( >>= )
  let ( >|= ) = ( >|= )
  let ( and+ ) = both
  let ( and* ) = both
  let ( let+ ) = ( >|= )
  let ( let* ) = ( >>= )
end

include Infix

let dict_field_or default name d =
  let+ r = dict_field_opt name d in
  match r with
  | Some r -> r
  | None -> default

let tup2 d1 d2 =
  let* l = list any in
  match l with
  | [ x1; x2 ] ->
    let+ x1 = apply d1 x1 and+ x2 = apply d2 x2 in
    x1, x2
  | _ -> fail "expected a pair"

let tup3 d1 d2 d3 =
  let* l = list any in
  match l with
  | [ x1; x2; x3 ] ->
    let+ x1 = apply d1 x1 and+ x2 = apply d2 x2 and+ x3 = apply d3 x3 in
    x1, x2, x3
  | _ -> fail "expected a triple"

let tup4 d1 d2 d3 d4 =
  let* l = list any in
  match l with
  | [ x1; x2; x3; x4 ] ->
    let+ x1 = apply d1 x1
    and+ x2 = apply d2 x2
    and+ x3 = apply d3 x3
    and+ x4 = apply d4 x4 in
    x1, x2, x3, x4
  | _ -> fail "expected a 4-tuple"

let dict_as_list =
  let+ d = dict in
  Str_map.to_list d

let rec map_l f = function
  | [] -> return []
  | x :: tl ->
    let* x = f x in
    let+ tl = map_l f tl in
    x :: tl

let rec fold_l f acc = function
  | [] -> return acc
  | x :: tl ->
    let* acc = f acc x in
    fold_l f acc tl

let[@inline] run d v = try Ok (d.deser v) with Fail err -> Error err
let[@inline] run_exn d v = d.deser v
