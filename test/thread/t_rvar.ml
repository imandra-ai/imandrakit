include (val Testlib.make ~__FILE__ ())
module Fut = Imandrakit_thread.Fut
module RV = Imandrakit_thread.Rvar

let () =
  t @@ fun () ->
  let x = RV.mk_var 0 in
  assert_equal ~printer:Q.Print.int 0 (RV.get x);
  RV.set x 1;
  assert_equal ~printer:Q.Print.int 1 (RV.get x);
  true

let () =
  t @@ fun () ->
  let x = RV.mk_var 0 in
  let y = RV.mk_var 1 in

  let sum = RV.monoid_merge (0, ( + )) ~f:Fun.id x y in
  assert_equal ~printer:Q.Print.int 1 (RV.get sum);

  RV.set x 8;
  assert_equal ~printer:Q.Print.int 9 (RV.get sum);

  RV.set y 14;
  assert_equal ~printer:Q.Print.int 22 (RV.get sum);
  true

let () =
  t @@ fun () ->
  let x = RV.mk_var 1 in
  let sum = RV.monoid_merge_l (0, ( + )) ~f:Fun.id [ x ] in
  assert_equal 1 (RV.get sum);
  RV.set x 3;
  assert_equal 3 (RV.get sum);
  true

let () =
  t ~name:__LOC__ @@ fun () ->
  let mk_sum l = RV.monoid_merge_l (0, ( + )) ~f:Fun.id l in

  let x = RV.mk_var 1 in
  let y = RV.mk_var 2 in
  let z = RV.mk_var 3 in
  let w = RV.mk_var 4 in
  let sum =
    mk_sum
      [
        mk_sum [ mk_sum [ x; y ] ];
        mk_sum [ mk_sum [ z ]; mk_sum [ mk_sum [ w ] ] ];
      ]
  in
  assert_equal ~printer:string_of_int 10 (RV.get sum);
  RV.set x 3;
  assert_equal ~printer:string_of_int 12 (RV.get sum);
  RV.set w 10;
  assert_equal ~printer:string_of_int 18 (RV.get sum);
  RV.set z 0;
  assert_equal ~printer:string_of_int 15 (RV.get sum);
  RV.set y 20;
  assert_equal ~printer:string_of_int 33 (RV.get sum);
  true

let () =
  t ~name:__LOC__ @@ fun () ->
  let vars = List.init 10 (fun i -> RV.mk_var i) in
  let sum = RV.monoid_merge_l (0, ( + )) ~f:Fun.id vars in

  assert_equal ~printer:Q.Print.int 45 (RV.get sum);
  List.iter (fun s -> RV.set s 1) vars;
  assert_equal ~printer:Q.Print.int 10 (RV.get sum);
  true

let () =
  t ~name:__LOC__ @@ fun () ->
  let fut = Fut.return 42 in
  let x = RV.of_fut fut in
  assert_equal (Some 42) (RV.get x);
  true

let () =
  t ~name:__LOC__ @@ fun () ->
  let fut, prom = Fut.make () in
  let x = RV.of_fut fut in
  assert_equal None (RV.get x);
  Fut.fulfill prom (Ok 41);
  assert_equal (Some 41) (RV.get x);
  true

let () =
  t ~name:__LOC__ @@ fun () ->
  let open RV.Infix in
  let v1 = RV.mk_var 1 in
  let v2 = RV.mk_var 2 in
  let tup =
    let* v1 = v1 in
    let* v2 = v2 in
    RV.return (v1, v2)
  in
  let sum =
    let* x, y = tup in
    RV.return (x + y)
  in

  assert_equal ~msg:__LOC__ ~printer:string_of_int 3 (RV.get sum);
  RV.set v1 10;
  assert_equal ~msg:__LOC__ ~printer:string_of_int 12 (RV.get sum);
  RV.freeze v1;
  assert_equal ~msg:__LOC__ ~printer:Q.Print.(pair int int) (10, 2) (RV.get tup);
  RV.set v2 20;
  assert_equal ~msg:__LOC__
    ~printer:Q.Print.(pair int int)
    (10, 20) (RV.get tup);
  assert_equal ~msg:__LOC__ ~printer:string_of_int 30 (RV.get sum);
  RV.freeze v2;
  true
