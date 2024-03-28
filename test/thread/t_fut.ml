include (val Imandrakit_testlib.make ~__FILE__ ())
module Fut = Imandrakit_thread.Fut

let () = eq 42 (Fut.wait_block_exn (Fut.return 42))

let () =
  eq (Error Not_found)
    (try
       let bt = Printexc.get_callstack 4 in
       Ok (Fut.wait_block_exn (Fut.fail Not_found bt))
     with e -> Error e)

let exec = Imandrakit_thread.Thread_pool.start ~name:"t_fut" ~j:4 ()

open Fut.Infix

let () =
  eq 120
    (Fut.wait_block_exn
       (let* x =
          Fut.spawn ~on:exec (fun () ->
              let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "sleep" in
              Thread.delay 0.1;
              2)
        and* y =
          Fut.spawn ~on:exec (fun () ->
              let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "sleep" in
              Thread.delay 0.1;
              60)
        in
        Fut.return (x * y)))

let () =
  let mysum = Iter.(0 -- 1_000 |> map (fun x -> x * x) |> sum) in
  let l =
    Fut.map_iter Iter.(0 -- 1_000) ~f:(fun x -> Fut.return (x * x))
    |> Fut.map ~f:Iter.sum
  in

  eq mysum (Fut.wait_block_exn l)
