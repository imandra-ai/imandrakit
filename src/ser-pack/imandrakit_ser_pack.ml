module V = Imandrakit_ser.Value

type value = V.t

let error : Error_kind.t = Error_kind.make ~name:"SerPackError" ()

(** Tag for pointers.
    [6] is not used, and fits in a single byte:
    See {{: https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml} the registry}. *)
let tag_ptr = 6

module V_tbl = Hashtbl.Make (V)
(** Main hashconsing table *)

module Ser = struct
  type ptr = value

  module type CACHE_KEY = sig
    include Hashtbl.HashedType

    val id : int
  end

  type 'a cache_key = (module CACHE_KEY with type t = 'a)
  type cache_key_with_val = K : 'a cache_key * 'a -> cache_key_with_val

  module Cache_tbl = Hashtbl.Make (struct
    type t = cache_key_with_val

    let equal (K ((module C1), v1)) (K ((module C2), v2)) : bool =
      C1.id = C2.id && C1.equal v1 (Obj.magic v2)

    let hash (K ((module C), v)) = C.hash v
  end)

  type state = {
    entries: value Vec.vector;
    hashcons: ptr V_tbl.t;
    cache: value Cache_tbl.t;
  }

  type 'a t = state -> 'a -> value

  let create () : state =
    {
      entries = Vec.create ();
      hashcons = V_tbl.create 16;
      cache = Cache_tbl.create 8;
    }

  let[@inline] mk_ptr_ n : value = V.tag tag_ptr (V.int n)
  let unit : value = V.null
  let int = V.int
  let int64 = V.int64
  let bool = V.bool
  let float = V.float
  let list = V.list
  let map = V.dict_of_list
  let string = V.string
  let bytes = V.bytes
  let list_of f st x = list (List.map (f st) x)

  let map_of fv st l : value =
    Iter.of_list l
    |> Iter.map (fun (k, v) -> k, fv st v)
    |> Str_map.of_iter |> V.dict

  let[@inline] delay f st x = f () st x

  let fix (f : 'a t -> 'a t) : 'a t =
    let rec _self = lazy (fun st x -> f (Lazy.force _self) st x) in
    Lazy.force _self

  let add_entry_hashcons (self : state) (c : value) : ptr =
    match c with
    | Tag (t, Int _) when t == tag_ptr -> c (* do not add pointers *)
    | Int _ | Bool _ | Null | Float _ -> c (* do not add scalars *)
    | _ ->
      (try V_tbl.find self.hashcons c
       with Not_found ->
         let n = Vec.length self.entries in
         Vec.push self.entries c;
         let ptr = mk_ptr_ n in
         V_tbl.add self.hashcons c ptr;
         ptr)

  let add_entry (self : state) (c : value) : ptr =
    match c with
    | Tag (t, Int _) when t == tag_ptr -> c (* do not add pointers *)
    | Int _ | Bool _ | Null | Float _ -> c (* do not add scalars *)
    | _ ->
      let n = Vec.length self.entries in
      Vec.push self.entries c;
      let ptr = mk_ptr_ n in
      ptr

  (** strings bigger than that will get their own entry *)
  let _hashcons_limit_str = 32

  let add_string ?(hashcons = false) self s : value =
    let c = string s in
    if hashcons || String.length s >= _hashcons_limit_str then
      add_entry_hashcons self c
    else
      c

  let add_bytes ?(hashcons = false) self b : value =
    let c = bytes b in
    if hashcons || String.length b >= _hashcons_limit_str then
      add_entry_hashcons self c
    else
      c

  let id_ = ref 0

  let create_cache_key (type a) (module H : Hashtbl.HashedType with type t = a)
      : a cache_key =
    let id =
      incr id_;
      !id_
    in
    (module struct
      include H

      let id = id
    end)

  let with_cache (key : 'a cache_key) (enc : 'a t) : 'a t =
   fun st (x : 'a) : value ->
    let k = K (key, x) in
    match Cache_tbl.find_opt st.cache k with
    | Some c -> c
    | None ->
      (* encode [x], and make sure it's an entry (or scalar) so we can reuse a
         pointer to it later *)
      let c = add_entry st (enc st x) in
      Cache_tbl.add st.cache k c;
      c

  let finalize_value (self : state) ~key : value =
    map [ "k", key; "h", V.list (Vec.to_list self.entries) ]

  let finalize_cbor_string (self : state) ~key : string =
    Imandrakit_ser_cbor.encode @@ finalize_value self ~key
end

let to_value (ser : 'a Ser.t) x : value =
  let st = Ser.create () in
  let key = ser st x in
  Ser.finalize_value st ~key

let to_cbor_string ser x =
  let st = Ser.create () in
  let key = ser st x in
  Ser.finalize_cbor_string st ~key

module Deser = struct
  type cached = ..

  module type CACHE_KEY = sig
    type elt
    type cached += C of elt
  end

  type 'a cache_key = (module CACHE_KEY with type elt = 'a)

  type state = {
    entries: value Vec.vector;  (** heap *)
    key: value;  (** entrypoint *)
    cache: cached V_tbl.t;
  }

  type 'a t = state -> value -> 'a

  let[@inline] return x _st _c = x
  let[@inline] fail s = Error.fail ~kind:error s
  let[@inline] failf s = Printf.ksprintf fail s

  type ptr = int64

  let[@inline] ptr_of_int x : ptr = Int64.of_int x

  let deref self (n : ptr) =
    let n = Int64.to_int n in
    if n >= Vec.length self.entries then
      fail "cbor_pack.deser.deref: invalid index";
    Vec.get self.entries n

  let rec deref_if_ptr self (x : value) : value =
    match x with
    | Tag (t, Int i) when t = tag_ptr ->
      (*Format.printf "deref %d@." i;*)
      deref_if_ptr self @@ deref self i
    | _ -> x

  let to_unit state c =
    match deref_if_ptr state c with
    | Null -> ()
    | _ -> fail "expected null"

  let to_int64_ = function
    | V.Int i -> i
    | _ -> fail "expected integer"

  let to_int state c = Int64.to_int @@ to_int64_ @@ deref_if_ptr state c
  let to_int64 state c = to_int64_ @@ deref_if_ptr state c

  let to_bool state c =
    match deref_if_ptr state c with
    | Bool x -> x
    | _ -> fail "expected bool"

  let to_float state c =
    match deref_if_ptr state c with
    | Float x -> x
    | _ -> fail "expected float"

  let to_list_ = function
    | V.List x -> x
    | _ -> fail "expected array"

  let to_list state c = to_list_ @@ deref_if_ptr state c

  let to_list_of f state c =
    match deref_if_ptr state c with
    | List x -> List.map (f state) x
    | _ -> fail "expected array"

  let to_map_no_deref_ = function
    | V.Dict l -> l
    | _ -> fail "expected map"

  let to_map state c = to_map_no_deref_ @@ deref_if_ptr state c
  let to_map_as_list state v = to_map state v |> Str_map.to_list

  let to_text state c =
    match deref_if_ptr state c with
    | V.Str x -> x
    | _ -> fail "expected text"

  let to_bytes state c =
    match deref_if_ptr state c with
    | V.Bytes x -> x
    | _ -> fail "expected bytes"

  let to_any_tag state c =
    match deref_if_ptr state c with
    | V.Tag (j, sub) -> j, sub
    | _ -> fail "expected (any) tag"

  let to_tag_ i = function
    | V.Tag (j, sub) when i = j -> sub
    | V.Tag _ -> fail "wrong tag"
    | _ -> failf "expected tag %d" i

  let to_tag i state c = to_tag_ i @@ deref_if_ptr state c
  let[@inline] to_ptr x : ptr = to_int64_ @@ to_tag_ tag_ptr x
  let[@inline] ( let+ ) self f state c = f (self state c)

  let[@inline] ( let* ) self (f : _ -> _ t) state c =
    let x = self state c in
    f x state c

  let[@inline] delay f st x = (f ()) st x

  let fix (f : 'a t -> 'a t) : 'a t =
    let rec _self = lazy (fun st x -> f (Lazy.force _self) st x) in
    Lazy.force _self

  let map_entry_no_deref_ ~k (c : value) : value =
    let m = to_map_no_deref_ c in
    try Str_map.find k m with Not_found -> fail "cannot find key in map"

  let map_entry ~k state (c : value) : value =
    let m = to_map state c in
    try Str_map.find k m with Not_found -> fail "cannot find key in map"

  let create_cache_key (type a) () : a cache_key =
    (module struct
      type elt = a
      type cached += C of a
    end)

  let with_cache (type a) (key : a cache_key) (dec : a t) : a t =
   fun st c ->
    let (module K) = key in
    let c = deref_if_ptr st c in
    match V_tbl.find_opt st.cache c with
    | Some (K.C v) -> v
    | Some _ -> dec st c
    | None ->
      let v = dec st c in
      V_tbl.add st.cache c (K.C v);
      v

  let[@inline] entry_key self = self.key

  let of_value_ v : state =
    (* NOTE: keep in touch with [Ser.finalize] *)
    let key = map_entry_no_deref_ ~k:"k" v in
    let entries = map_entry_no_deref_ ~k:"h" v |> to_list_ |> Vec.of_list in
    let cache = V_tbl.create 8 in
    { entries; key; cache }

  let parse s : state Error.result =
    try
      let c = Imandrakit_ser_cbor.decode_exn s in
      Ok (of_value_ c)
    with
    | Error.E err -> Error err
    | exn ->
      let bt = Printexc.get_raw_backtrace () in
      let err = Error.of_exn ~kind:error ~bt exn in
      Error err

  let parse_exn s = parse s |> Error.unwrap

  let pp_diagnostic out self =
    Format.fprintf out "{@[<hv>h=[@[<hv>";
    Vec.iteri
      (fun i x ->
        let x =
          match x with
          | V.Str s when String.length s > 100 ->
            V.string
              (Printf.sprintf "%s… (%d more bytes)" (String.sub s 0 100)
                 (String.length s - 100))
          | V.Bytes s when String.length s > 50 ->
            V.bytes
              (Printf.sprintf "%S… (%d more bytes)" (String.sub s 0 50)
                 (String.length s - 50))
          | x -> x
        in
        Format.fprintf out "@[%d: %a@];@ " i V.pp x)
      self.entries;
    Format.fprintf out "@]];@ k=%a@]}" V.pp self.key

  let show_diagnostic self =
    let buf = Buffer.create 32 in
    Format.fprintf (Format.formatter_of_buffer buf) "%a@?" pp_diagnostic self;
    Buffer.contents buf
end

let pp_diagnostic out (v : value) =
  match Deser.of_value_ v with
  | deser -> Deser.pp_diagnostic out deser
  | exception _ -> V.pp out v

let of_value_exn deser h =
  let st = Deser.of_value_ h in
  let key = Deser.entry_key st in
  deser st key

let of_value deser h =
  try Ok (of_value_exn deser h) with
  | Error.E err -> Error err
  | exn ->
    let bt = Printexc.get_raw_backtrace () in
    let err = Error.of_exn ~bt ~kind:error exn in
    Error err

let of_cbor_string_exn deser h =
  let st = Deser.parse_exn h in
  let key = Deser.entry_key st in
  deser st key

let of_cbor_string deser h =
  try Ok (of_cbor_string_exn deser h) with
  | Error.E err -> Error err
  | exn ->
    let bt = Printexc.get_raw_backtrace () in
    let err = Error.of_exn ~bt ~kind:error exn in
    Error err
