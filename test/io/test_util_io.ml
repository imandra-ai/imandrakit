include (val Testlib.make ~__FILE__ ())
module U = Imandrakit_io

let () =
  t (fun () ->
      let filename = ".test-util-io" in
      let@ () =
        Fun.protect ~finally:(fun () -> try Sys.remove filename with _ -> ())
      in

      (let@ () = U.with_capture_stdio_file ~file:filename ~append:false () in
       Printf.printf "hello\n%!");

      assert_bool "file must exist" (Sys.file_exists filename);
      assert_equal ~printer:(spf "%S") "hello\n" (CCIO.File.read_exn filename);
      true)

let () =
  t ~name:"capture string ok" (fun () ->
      let i, out, err =
        let@ () = U.with_capture_stdio_string () in
        Printf.printf "hello\n%!";
        Printf.eprintf "oh no%!";
        42
      in

      assert_equal (Ok 42) i;
      assert_equal ~printer:(spf "%S") "hello\n" out;
      assert_equal ~printer:(spf "%S") "oh no" err;
      true)

let () =
  t ~name:"capture string err" (fun () ->
      let s_out = ref "" in
      let s_err = ref "" in
      let i, out, err =
        let@ () =
          U.with_capture_stdio_string
            ~on_stdout:(fun b i len ->
              s_out := !s_out ^ Bytes.sub_string b i len)
            ~on_stderr:(fun b i len ->
              s_err := !s_err ^ Bytes.sub_string b i len)
            ()
        in
        Printf.printf "hello\n%!";
        Printf.eprintf "oh no%!";
        raise Exit
      in

      assert_equal (Error Exit) i;
      assert_equal ~printer:(spf "%S") "hello\n" out;
      assert_equal ~printer:(spf "%S") "oh no" err;
      assert_equal ~printer:(spf "%S") "hello\n" !s_out;
      assert_equal ~printer:(spf "%S") "oh no" !s_err;
      true)

let () =
  t ~name:"capture string large" (fun () ->
      let expect = Buffer.create 32 in
      let i, out, err =
        let@ () = U.with_capture_stdio_string () in

        for _i = 0 to 10_000 do
          Printf.printf "hello %d\n%!" _i;
          Printf.bprintf expect "hello %d\n%!" _i
        done
      in

      assert_equal (Ok ()) i;
      assert_equal ~printer:(spf "%S") (Buffer.contents expect) out;
      assert_equal ~printer:(spf "%S") "" err;
      true)

let () =
  t ~name:"capture string many (no FD leak)" (fun () ->
      let n = 200 in
      let l =
        List.init n (fun i ->
            let _, out, err =
              let@ () = U.with_capture_stdio_string () in

              Printf.printf "hello %d\n%!" i;
              ()
            in
            out, err)
        |> List.map fst
      in

      assert_equal
        ~printer:(fun l -> String.concat "," @@ List.map (spf "%S") l)
        (List.init n (fun i -> spf "hello %d\n" i))
        l;
      true)

(* run multiple threads, making sure only one at a time can
   redirect stdio *)
let () =
  t ~name:"capture string multi thread" (fun () ->
      let threads =
        List.init 10 (fun _ ->
            Thread.create
              (fun () ->
                let n = 20 in
                let l =
                  List.init n (fun i ->
                      let _, out, err =
                        let@ () = U.with_capture_stdio_string () in

                        Printf.printf "hello %d\n%!" i;
                        ()
                      in
                      out, err)
                  |> List.map fst
                in

                assert_equal
                  ~printer:(fun l -> String.concat "," @@ List.map (spf "%S") l)
                  (List.init n (fun i -> spf "hello %d\n" i))
                  l)
              ())
      in
      List.iter Thread.join threads;
      true)
