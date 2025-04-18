include (val Imandrakit_testlib.make ~__FILE__ ())
module Fut = Imandrakit_thread.Fut
module T = Imandrakit_thread.Timer

let ( ! ) = Atomic.get
let ( := ) = Atomic.set

let () =
  t @@ fun () ->
  let timer = T.create () in
  let x = Atomic.make false in
  let y = Atomic.make false in

  T.run_after_s timer 1.0 (fun () -> y := true);
  T.run_after_s timer 0.5 (fun () -> x := true);

  Thread.delay 0.01;
  assert_equal ~msg:"0.01 x" false !x;
  assert_equal ~msg:"0.01 y" false !y;

  Thread.delay 0.74;
  assert_equal ~msg:"0.75 x" true !x;
  assert_equal ~msg:"0.75 y" false !y;

  Thread.delay 0.45;
  assert_equal ~msg:"1.2 x" true !x;
  assert_equal ~msg:"1.2 y" true !y;
  true

let () =
  t @@ fun () ->
  let timer = T.create () in
  let i = Atomic.make 0 in
  let h = T.run_every_s' timer ~initial:0.5 0.3 (fun () -> Atomic.incr i) in

  assert (!i = 0);
  Thread.delay 0.2;
  assert (!i = 0);
  Thread.delay 0.4;
  assert (!i = 1);
  Thread.delay 0.3;
  assert (!i = 2);
  Thread.delay 0.3;
  assert (!i = 3);

  T.cancel timer h;
  for _i = 1 to 4 do
    Thread.delay 0.1;
    assert (!i = 3)
  done;

  true

let () =
  t @@ fun () ->
  let r = Atomic.make 0 in
  let sum = Atomic.make 0 in
  let timer = T.create () in
  let fut, prom = Fut.make () in

  T.run_every_s timer 0.0001 (fun () ->
      if !r = 10 then (
        Fut.fulfill prom @@ Ok ();
        raise T.Stop_timer
      );

      Atomic.incr r;
      ignore (Atomic.fetch_and_add sum !r : int));

  Fut.wait_block_exn fut;
  Thread.yield ();
  assert (!sum = 55);
  true
