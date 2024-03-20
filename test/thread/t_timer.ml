include (val Testlib.make ~__FILE__ ())
module T = Imandrakit_thread.Timer

let timer = T.create ()
let ( ! ) = Atomic.get
let ( := ) = Atomic.set

let () =
  t (fun () ->
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
      true)
