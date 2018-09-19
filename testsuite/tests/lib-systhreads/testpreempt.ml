(* TEST
   * hassysthreads
   (*
     On Windows, we use Sleep(0) for triggering preemption of threads.
     However, this does not seem very reliable, so that this test fails
     on some Windows configurations. See GPR #1533.
   *)
   include systhreads
   ** not-windows
   *** bytecode
   *** native
*)

let rec generate_list n =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (float n :: acc) (n-1)
  in
  aux [] n

let rec long_computation time0 =
  let long_list = generate_list 100000 in
  let res = List.length (List.rev_map sin long_list) in
  if Sys.time () -. time0 > 2. then
    Printf.printf "Long computation result: %d\n%!" res
  else long_computation time0

let interaction () =
  Thread.delay 0.1;
  Printf.printf "Interaction 1\n";
  Thread.delay 0.1;
  Printf.printf "Interaction 2\n"

let () =
  ignore (Thread.create interaction ());
  long_computation (Sys.time ())
