(* run this with magic-trace to see *)

let ( let@ ) = ( @@ )

let rec fib i =
  if i <= 2 then
    1
  else
    fib (i - 1) + fib (i - 2)

let[@inline never] run n = Printf.printf "fib %d = %d\n%!" n (fib n)

let[@inline never] loop_iter () =
  let@ () = Imandrakit_magic_trace.with_snapshot in
  run 2;
  run 44;
  run 8;
  run 26

let () =
  for _i = 1 to 4 do
    loop_iter ();
    Unix.sleepf 0.02
  done;
  run 40;
  Unix.sleepf 0.5;
  ()
