module U = Imandrakit_io

let assert_equal ?printer a b : unit =
  if a = b then
    ()
  else (
    let msg =
      match printer with
      | Some f -> Fmt.asprintf "not equal: %s vs %s@." (f a) (f b)
      | None -> "not equal"
    in
    failwith msg
  )

let test () =
  let input = "hello world" in
  let i_idx = ref 0 in
  let out = Buffer.create 32 in

  let _stdin, _stdout =
    U.dup_stdin_stdout
      ~on_stdin:(fun buf i len ->
        let len = min len (String.length input - !i_idx) in
        Bytes.blit_string input !i_idx buf i len;
        i_idx := !i_idx + len;
        len)
      ~on_stdout:(fun b i len -> Buffer.add_subbytes out b i len)
      ()
  in

  (* this ends up in an .out file *)
  Printf.fprintf _stdout "testing old stdout\n%!";

  let read_from_stdin = CCIO.read_all stdin in
  assert_equal ~printer:(spf "%S") input read_from_stdin;

  Printf.printf "trying to write stuff%!";
  Thread.delay 0.2;
  (* time to write through the pipe *)
  assert_equal ~printer:(spf "%S") "trying to write stuff" (Buffer.contents out);

  (* now read old stdin (from .in file) and copy to old stdout (the .out file) *)
  let l = CCIO.read_lines_l _stdin in
  List.iter (Printf.fprintf _stdout "from stdin: %s\n") l;

  ()

let () =
  try test ()
  with Failure msg ->
    Printf.eprintf "failure: %s\n%!" msg;
    exit 1
