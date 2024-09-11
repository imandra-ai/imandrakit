module Debug = Imandrakit_debug
module Trace = Trace_core

let ( let@ ) = ( @@ )

let loop3 i j : unit =
  let@ _sp =
    Trace.with_span ~__FILE__ ~__LINE__ "loop3" ~data:(fun () ->
        [ "i", `Int i; "j", `Int j ])
  in
  Thread.delay 0.001

let loop2 i : unit =
  let@ _sp =
    Trace.with_span ~__FILE__ ~__LINE__ "loop2" ~data:(fun () ->
        [ "i", `Int i ])
  in
  Logs.debug (fun k -> k "loop2");
  for j = 1 to 5 do
    loop3 i j;
    Thread.delay 0.05
  done;
  ()

let loop1 () =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "loop1" in
  while true do
    Logs.debug (fun k -> k "loop1");
    Trace.message "hello";

    for i = 1 to 5 do
      loop2 i
    done;

    Thread.delay 0.1;

    Debug.suspend ()
  done

let setup_logs () =
  Logs.set_reporter @@ Logs.format_reporter ();
  let lvl =
    if Sys.getenv_opt "DEBUG" |> Option.is_some then
      Logs.Debug
    else
      Logs.Info
  in
  Logs.set_level ~all:true (Some lvl)

let () =
  setup_logs ();
  Debug.setup ();
  loop1 ()
