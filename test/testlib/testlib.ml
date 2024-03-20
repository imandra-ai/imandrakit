module Q = QCheck
module Fmt = CCFormat

let spf = Printf.sprintf

type 'a eq = 'a -> 'a -> bool

type 'a print = 'a -> string

let () =
  Printexc.register_printer (function
    | Failure s -> Some ("failure: " ^ s)
    | _ -> None)

module Test = struct
  type run =
    | T of { prop: unit -> bool }
    | Eq : {
        eq: 'a eq option;
        print: 'a print option;
        lhs: 'a;
        rhs: 'a;
        neg: bool;
      }
        -> run
    | Q : {
        count: int option;
        arb: 'a Q.arbitrary;
        prop: 'a -> bool;
        long_factor: int option;
        max_gen: int option;
        max_fail: int option;
        if_assumptions_fail: ([ `Fatal | `Warning ] * float) option;
      }
        -> run

  type t = {
    name: string option;
    run: run;
    __FILE__: string;
    n: int;
  }

  (** Location for this test *)
  let str_loc (self : t) : string =
    let what =
      match self.name with
      | None -> ""
      | Some f -> spf " :name %S" f
    in
    Printf.sprintf "(test :file '%s'%s :n %d)" self.__FILE__ what self.n

  let str_short (self : t) : string =
    match self.name with
    | None -> spf "'%s:%d'" self.__FILE__ self.n
    | Some f -> spf "'%s:%s'" self.__FILE__ f

  let get_state (r : _ QCheck.TestResult.t) : _ QCheck.TestResult.state =
    QCheck.TestResult.get_state r

  let run ?(long = false) ~seed (self : t) : _ result =
    match
      match self.run with
      | T { prop } ->
        let fail msg = Error (spf "failed: %s" msg) in

        (match prop () with
        | exception e -> fail (spf "raised %s" (Printexc.to_string e))
        | true -> Ok ()
        | false -> fail "returns false")
      | Eq { eq; print; lhs; rhs; neg } ->
        let eq =
          match eq with
          | Some f -> f
          | None -> ( = )
        in
        if neg then
          if not (eq lhs rhs) then
            Ok ()
          else (
            let msg =
              match print with
              | None -> "failed: equal"
              | Some p -> spf "failed: equal:\nlhs=%s\nrhs=%s" (p lhs) (p rhs)
            in
            Error msg
          )
        else if eq lhs rhs then
          Ok ()
        else (
          let msg =
            match print with
            | None -> "failed: not equal"
            | Some p -> spf "failed: not equal:\nlhs=%s\nrhs=%s" (p lhs) (p rhs)
          in
          Error msg
        )
      | Q
          {
            count;
            arb;
            prop;
            long_factor;
            max_fail;
            max_gen;
            if_assumptions_fail;
          } ->
        (* create a random state from the seed *)
        let rand =
          let bits =
            CCString.to_list seed |> List.map Char.code |> Array.of_list
          in
          Random.State.make bits
        in

        let module Fmt = CCFormat in
        let cell =
          Q.Test.make_cell ?if_assumptions_fail ?max_gen ?max_fail ?count
            ?long_factor ~name:(str_loc self) arb prop
        in

        let pp_cex out (cx : _ Q.TestResult.counter_ex) =
          let { Q.TestResult.instance; shrink_steps = n; msg_l } = cx in
          let msg_l =
            if msg_l = [] then
              ""
            else
              "\n" ^ String.concat "\n" msg_l
          in
          match arb.print with
          | None ->
            Fmt.fprintf out "<instance> (after %d shrink steps)%s" n msg_l
          | Some p ->
            Fmt.fprintf out "`%s` (after %d shrink steps)%s" (p instance) n
              msg_l
        in

        (* TODO: if verbose, print stats, etc. *)
        let res = Q.Test.check_cell ~long ~rand cell in

        (match get_state res with
        | QCheck.TestResult.Success -> Ok ()
        | QCheck.TestResult.Failed { instances } ->
          let msg =
            Format.asprintf "@[<v2>failed on instances:@ %a@]"
              (Fmt.list ~sep:(Fmt.return ";@ ") pp_cex)
              instances
          in
          Error msg
        | QCheck.TestResult.Failed_other { msg } ->
          let msg = spf "failed: %s" msg in
          Error msg
        | QCheck.TestResult.Error { instance; exn; backtrace } ->
          let msg = Printexc.to_string exn in
          let msg =
            Format.asprintf "@[<v2>raised %s@ on instance %a@ :backtrace %s@]"
              msg pp_cex instance backtrace
          in
          Error msg)
    with
    | res -> res
    | exception e -> Error (spf "failed: raised %s" (Printexc.to_string e))
end

module type S = sig
  module Q = QCheck

  val t : ?name:string -> (unit -> bool) -> unit

  val eq : ?name:string -> ?cmp:'a eq -> ?printer:'a print -> 'a -> 'a -> unit

  val neq : ?name:string -> ?cmp:'a eq -> ?printer:'a print -> 'a -> 'a -> unit

  val q :
    ?name:string ->
    ?count:int ->
    ?long_factor:int ->
    ?max_gen:int ->
    ?max_fail:int ->
    ?if_assumptions_fail:[ `Fatal | `Warning ] * float ->
    'a Q.arbitrary ->
    ('a -> bool) ->
    unit

  val assert_equal :
    ?msg:string ->
    ?printer:('a -> string) ->
    ?cmp:('a -> 'a -> bool) ->
    'a ->
    'a ->
    unit

  val assert_bool : string -> bool -> unit

  val assert_failure : string -> 'a

  val assert_raises : (exn -> bool) -> (unit -> 'b) -> unit

  val get : unit -> Test.t list
end

module Make_test (X : sig
  val file : string
end) =
struct
  module Q = QCheck

  let all_ : Test.t list ref = ref []

  let add_ t = all_ := t :: !all_

  let n_ = ref 0

  let mk ?name run : Test.t =
    let n = !n_ in
    incr n_;
    { __FILE__ = X.file; name; n; run }

  let t ?name f : unit = add_ @@ mk ?name @@ Test.T { prop = f }

  let eq ?name ?cmp ?printer lhs rhs : unit =
    add_ @@ mk ?name
    @@ Test.Eq { eq = cmp; print = printer; lhs; rhs; neg = false }

  let neq ?name ?cmp ?printer lhs rhs : unit =
    add_ @@ mk ?name
    @@ Test.Eq { eq = cmp; print = printer; lhs; rhs; neg = true }

  let q ?name ?count ?long_factor ?max_gen ?max_fail ?if_assumptions_fail arb
      prop : unit =
    add_ @@ mk ?name
    @@ Test.Q
         {
           arb;
           prop;
           count;
           long_factor;
           max_gen;
           max_fail;
           if_assumptions_fail;
         }

  let assert_equal ?msg ?printer ?(cmp = ( = )) x y : unit =
    if not @@ cmp x y then (
      match msg, printer with
      | None, None -> failwith "not equal"
      | Some msg, None -> failwith (spf "not equal: %s" msg)
      | None, Some p -> failwith @@ spf "not equal: lhs=%s, rhs=%s" (p x) (p y)
      | Some msg, Some p ->
        failwith @@ spf "not equal: %s: lhs=%s, rhs=%s" msg (p x) (p y)
    )

  let assert_bool what b = if not b then failwith what

  let assert_failure s = failwith s

  let assert_raises check f =
    try
      ignore (f ());
      failwith "did not raise"
    with e ->
      if check e then
        ()
      else
        failwith ("raised unexpected exception " ^ Printexc.to_string e)

  let get () = !all_
end

(** Build a test interface for a given module.

    Don't forget to obtain the test interface via [get()] to pass it
    to the test runner. *)
let make ~__FILE__ () : (module S) =
  let module M = Make_test (struct
    let file = __FILE__
  end) in
  (module M)

let getenv_opt s = try Some (Sys.getenv s) with _ -> None

let env_true s =
  match getenv_opt s with
  | Some ("true" | "1") -> true
  | _ -> false

let long = env_true "LONG"

let verbose = env_true "VERBOSE"

let run_all ?seed:seed_hex ?(long = long) ~descr (l : Test.t list list) : unit =
  let start = Mtime_clock.now () in
  let count = ref 0 in

  (* generate or parse seed *)
  let seed_hex =
    match seed_hex, getenv_opt "SEED" with
    | Some s, _ -> s
    | None, Some s -> s
    | None, None ->
      Random.self_init ();
      let a = CCList.init 8 (fun _ -> Random.int 256 |> Char.chr) in
      CCString.to_hex @@ CCString.of_list a
  in

  let seed =
    match CCString.of_hex seed_hex with
    | Some s -> s
    | None ->
      Format.printf "error: seed must be a hex string: %S@." seed_hex;
      exit 1
  in

  Format.printf "seed: %s@." seed_hex;

  (* now run the suite *)
  let suite = List.flatten l in
  Format.printf "testing '%s': running %d tests…@." descr (List.length suite);
  let failed = ref [] in

  List.iter
    (fun t ->
      incr count;
      if verbose then Format.printf "> run %s@." (Test.str_short t);
      match Test.run ~long ~seed t with
      | Ok () -> ()
      | Error msg ->
        Format.printf "FAILED: %s@." (Test.str_short t);
        failed := (t, msg) :: !failed)
    suite;

  let elapsed =
    let t_ns =
      Mtime.span (Mtime_clock.now ()) start |> Mtime.Span.to_float_ns
    in
    t_ns /. 1e9
  in
  Format.printf "%d tests done in %.3fs for %s@." !count elapsed descr;

  match !failed with
  | [] -> Format.printf "OK@."
  | _f ->
    let n_fail = List.length _f in
    Format.printf "ERROR (%d failures)@." n_fail;
    List.iteri
      (fun i (t, msg) ->
        Format.printf "@.========@.@[<2>failed (%d/%d) %s:@ %a@]@." (i + 1)
          n_fail (Test.str_short t) Fmt.string_lines msg)
      _f;
    exit 1
