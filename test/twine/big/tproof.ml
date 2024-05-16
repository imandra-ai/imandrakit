[@@@ocaml.warning "-32"]

type term = {
  t_id: int;
  t_view: term_view;
}

and term_view =
  | Const of int
  | App of term * term
  | Plus of term * term
[@@deriving show, twine, serpack] [@@hashcons]

type proof = {
  p_id: int;
  p_view: proof_view;
}

and proof_view =
  | MP of proof * proof
  | Assume of term
  | Refl of term
[@@deriving show, twine, serpack] [@@hashcons]

let () =
  Imandrakit_twine.Encode.add_cache_with
    ~eq:(fun a b -> a.t_id = b.t_id)
    ~hash:(fun t -> t.t_id)
    term_to_twine_ref;
  Imandrakit_twine.Decode.add_cache term_of_twine_ref;
  Imandrakit_twine.Encode.add_cache_with
    ~eq:(fun a b -> a.p_id = b.p_id)
    ~hash:(fun p -> p.p_id)
    proof_to_twine_ref;
  Imandrakit_twine.Decode.add_cache proof_of_twine_ref;
  ()

open struct
  let mk_term : term_view -> term =
    let hcons = Hashtbl.create 8 in
    let n = ref 0 in
    fun view ->
      try Hashtbl.find hcons view
      with Not_found ->
        let t = { t_id = !n; t_view = view } in
        incr n;
        Hashtbl.add hcons view t;
        t

  let mk_proof : proof_view -> proof =
    let hcons = Hashtbl.create 8 in
    let n = ref 0 in
    fun view ->
      try Hashtbl.find hcons view
      with Not_found ->
        let p = { p_id = !n; p_view = view } in
        incr n;
        Hashtbl.add hcons view p;
        p
end

let rec gen_term st i : term =
  mk_term
  @@
  if i <= 0 then
    Const (Random.State.int st 10)
  else (
    let lhs, rhs = gen_term st (i - 1), gen_term st (i - 2) in
    if Random.State.bool st then
      App (lhs, rhs)
    else
      Plus (lhs, rhs)
  )

let rec gen_proof st i : proof =
  mk_proof
  @@
  if i <= 0 then
    if Random.State.bool st then
      Assume (gen_term st 3)
    else
      Refl (gen_term st 4)
  else (
    let lhs, rhs = gen_proof st (i - 1), gen_proof st (i - 2) in
    MP (lhs, rhs)
  )

let psize p : int =
  let tbl = Hashtbl.create 16 in
  let n = ref 0 in
  let rec loop p =
    if not (Hashtbl.mem tbl p.p_id) then (
      incr n;
      match p.p_view with
      | MP (a, b) ->
        loop a;
        loop b
      | Assume _ | Refl _ -> ()
    )
  in
  loop p;
  !n

(* XXX: disable this for normal testing, to be deterministic *)
let enable_time = ref false

let timeit what f =
  let t = Unix.gettimeofday () in
  let res = f () in
  if !enable_time then
    Format.printf "time for %s: %.3fs@." what (Unix.gettimeofday () -. t);
  res

let st = Random.State.make [| 42 |]
let p = gen_proof st 20
let () = Format.printf "size(p): %d@." (psize p)

let p_twine =
  timeit "twine" @@ fun () -> Imandrakit_twine.Encode.encode_to_string proof_to_twine p

(* XXX: full dump, for debug *)
let enable_dump = ref false

let () =
  if !enable_dump then
    Format.printf "dump:@.%s@." (Imandrakit_twine.Dump.dump_string p_twine)

let () = Format.printf "twine(p): %d B@." (String.length p_twine)

let p_serpack =
  timeit "serpack" @@ fun () ->
  Imandrakit_ser_pack.to_cbor_string proof_to_serpack p

let () = Format.printf "serpack(p): %d B@." (String.length p_serpack)
let p_marshal = timeit "marshal" @@ fun () -> Marshal.to_string p []
let () = Format.printf "marshal(p): %d B@." (String.length p_marshal)

(* decoding *)

let p2 =
  timeit "decode_twine" @@ fun () ->
  Imandrakit_twine.Decode.decode_string proof_of_twine p_twine

let (_ : proof) =
  timeit "decode_serpack" @@ fun () ->
  Imandrakit_ser_pack.of_cbor_string_exn proof_of_serpack p_serpack

let (_ : proof) =
  timeit "decode_marshall" @@ fun () -> Marshal.from_string p_marshal 0

let () = Format.printf "size(p2): %d@." (psize p2)
