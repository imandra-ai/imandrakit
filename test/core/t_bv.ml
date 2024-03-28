include (val Imandrakit_testlib.make ~__FILE__ ())
module BV = Basic_bv

let () =
  t ~name:"t1" @@ fun () ->
  let bv = BV.create 4 in
  assert_equal ~printer:string_of_bool false (BV.get bv 0);
  assert_equal ~printer:string_of_bool false (BV.get bv 1);
  assert_equal ~printer:string_of_bool false (BV.get bv 2);
  assert_equal ~printer:string_of_bool false (BV.get bv 3);
  assert_equal 8 (BV.length bv);
  true

let () =
  t ~name:"t2" @@ fun () ->
  let bv = BV.create 1024 in
  assert_equal ~printer:string_of_bool false (BV.get bv 0);
  assert_equal ~printer:string_of_bool false (BV.get bv 1);
  assert_equal ~printer:string_of_bool false (BV.get bv 2);
  assert_equal ~printer:string_of_bool false (BV.get bv 3);
  assert_equal ~printer:string_of_bool false (BV.get bv 997);
  assert_equal ~printer:string_of_bool false (BV.get bv 998);
  assert_equal ~printer:string_of_bool false (BV.get bv 999);
  assert_equal ~printer:string_of_bool false (BV.get bv 1023);
  BV.set bv 998;
  assert_equal ~printer:string_of_bool false (BV.get bv 0);
  assert_equal ~printer:string_of_bool false (BV.get bv 1);
  assert_equal ~printer:string_of_bool false (BV.get bv 2);
  assert_equal ~printer:string_of_bool false (BV.get bv 3);
  assert_equal ~printer:string_of_bool false (BV.get bv 997);
  assert_equal ~printer:string_of_bool true (BV.get bv 998);
  assert_equal ~printer:string_of_bool false (BV.get bv 999);
  assert_equal ~printer:string_of_bool false (BV.get bv 1023);
  BV.set bv 0;
  assert_equal ~printer:string_of_bool true (BV.get bv 0);
  assert_equal ~printer:string_of_bool false (BV.get bv 1);
  assert_equal ~printer:string_of_bool false (BV.get bv 2);
  assert_equal ~printer:string_of_bool false (BV.get bv 3);
  BV.set bv 3;
  assert_equal ~printer:string_of_bool true (BV.get bv 0);
  assert_equal ~printer:string_of_bool false (BV.get bv 1);
  assert_equal ~printer:string_of_bool false (BV.get bv 2);
  assert_equal ~printer:string_of_bool true (BV.get bv 3);
  assert_equal ~printer:string_of_bool false (BV.get bv 4);
  true

let () =
  t ~name:"t3" @@ fun () ->
  let bv = BV.of_list [ 1; 5; 225; 1023; 1024; 2048; 4095; 4097 ] in
  assert_equal ~printer:string_of_bool false (BV.get bv 0);
  assert_equal ~printer:string_of_bool true (BV.get bv 1);
  assert_equal ~printer:string_of_bool false (BV.get bv 2);
  assert_equal ~printer:string_of_bool false (BV.get bv 3);
  assert_equal ~printer:string_of_bool false (BV.get bv 4);
  assert_equal ~printer:string_of_bool true (BV.get bv 5);
  assert_equal ~printer:string_of_bool false (BV.get bv 6);
  assert_equal ~printer:string_of_bool false (BV.get bv 7);
  for i = 8 to 224 do
    assert_equal ~printer:string_of_bool false (BV.get bv i)
  done;
  assert_equal ~printer:string_of_bool true (BV.get bv 225);
  for i = 226 to 1022 do
    assert_equal ~printer:string_of_bool false (BV.get bv i)
  done;
  assert_equal ~printer:string_of_bool true (BV.get bv 1023);
  assert_equal ~printer:string_of_bool true (BV.get bv 1024);
  for i = 1025 to 2047 do
    assert_equal ~printer:string_of_bool false (BV.get bv i)
  done;
  assert_equal ~printer:string_of_bool true (BV.get bv 2048);
  for i = 2049 to 4094 do
    assert_equal ~printer:string_of_bool false (BV.get bv i)
  done;
  assert_equal ~printer:string_of_bool true (BV.get bv 4095);
  assert_equal ~printer:string_of_bool false (BV.get bv 4096);
  assert_equal ~printer:string_of_bool true (BV.get bv 4097);
  true

let () =
  t ~name:"to_list1" @@ fun () ->
  let bv = BV.of_list [ 72 ] in
  assert_equal ~printer:Q.Print.(list int) [ 72 ] (BV.to_list bv);
  true

let () =
  q ~name:"of_list_to_list"
    Q.(small_list small_nat)
    (fun l ->
      let l = List.sort_uniq compare l in
      let bv = BV.of_list l in
      BV.to_list bv |> List.sort compare = l)

let () =
  q ~name:"of_list_get"
    Q.(small_list small_nat)
    (fun l ->
      let l = List.sort_uniq compare l in
      let bv = BV.of_list l in

      let max = List.fold_left max 0 l in
      for i = 0 to max - 1 do
        assert (BV.get bv i = List.mem i l)
      done;
      true)
