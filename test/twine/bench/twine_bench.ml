[@@@ocaml.warning "-32"]

let spf = Printf.sprintf

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

(* XXX: disable this for normal testing, to be deterministic *)
let enable_time = ref true

let timeit ?(n = 1) what f =
  let t = Unix.gettimeofday () in
  let res = f () in
  if !enable_time then (
    let elapsed = Unix.gettimeofday () -. t in
    Format.printf "time for %s: %.3fs (%.3fs/iter)@." what elapsed
      (elapsed /. float n)
  );
  res

let make_twine () =
  let st = Random.State.make [| 42 |] in
  let p = gen_proof st 20 in
  Imandrakit_twine.Encode.encode_to_string proof_to_twine p

let () =
  match Sys.argv.(1) with
  | "gen" ->
    let s = timeit "twine" @@ fun () -> make_twine () in
    CCIO.File.write_exn "data.twine" s
  | "parse" ->
    let s = CCIO.File.read_exn "data.twine" in
    Gc.full_major ();

    let stat1 = Gc.quick_stat () in

    let n = 20 in
    timeit ~n (spf "decoding %d times" n) @@ fun () ->
    for _i = 1 to n do
      let (_ : proof) =
        Sys.opaque_identity
          (Imandrakit_twine.Decode.decode_string proof_of_twine s)
      in
      ()
    done;

    let stat2 = Gc.quick_stat () in
    Format.printf "allocated %a minor, %a major@." Imandrakit.Util.pp_byte_size
      (int_of_float (stat2.minor_words -. stat1.minor_words))
      Imandrakit.Util.pp_byte_size
      (int_of_float (stat2.major_words -. stat1.major_words))
  | _ -> failwith "expect parse|gen"
