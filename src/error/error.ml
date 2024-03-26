include Error_core
module Kind = Kind

let pp_ ~f fmt = Fmt.kasprintf f fmt

let mk_error ?(stack = []) ?(process = Internal_.get_process_name ()) ?bt ~kind
    msg : t =
  let bt =
    match bt with
    | Some _ as b -> b
    | None when Printexc.backtrace_status () -> Some (Printexc.get_backtrace ())
    | None -> None
  in
  { msg = { msg; data = Data.empty; bt }; kind; stack; process }

let fail ?stack ?process ?bt ~kind msg =
  raise_err (mk_error ?stack ?process ?bt ~kind msg)

let mk_errorf ?stack ?process ?bt ~kind fmt =
  pp_ fmt ~f:(fun msg -> mk_error ?stack ?process ?bt ~kind msg)

let failf ?stack ?process ?bt ~kind fmt =
  pp_ fmt ~f:(fun s -> raise_err (mk_error ?stack ?process ?bt ~kind s))

let message ?bt msg : message = { msg; data = Data.empty; bt }
let messagef ?bt fmt = Format.kasprintf (message ?bt) fmt

let of_exn ?bt ~kind exn : t =
  match exn with
  | E e -> e
  | _ ->
    let bt = Option.map Printexc.raw_backtrace_to_string bt in
    mk_error ?bt ~kind (Printexc.to_string exn)

let[@inline] guardf ?let_pass k f =
  guard ?let_pass (fun () -> k (fun fmt -> Fmt.kasprintf message fmt)) f

let[@inline] guards ?let_pass msg f = guard ?let_pass (fun () -> message msg) f

let try_catch ~kind () f : _ result =
  try Ok (f ()) with
  | E err -> Error err
  | exn ->
    let bt = Printexc.get_raw_backtrace () in
    let err = of_exn_ ~kind ~bt exn in
    Error err

let unwrap_opt = function
  | Some x -> x
  | None ->
    fail ~kind:Kind.generic_internal_error "Expected `Some _`, got `None`."
