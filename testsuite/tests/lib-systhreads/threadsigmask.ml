(* TEST
   (*
     Thread.sigmask is not available on Windows
   *)
   include systhreads
   * not-windows
   ** bytecode
   ** native
*)

exception Exc

(* A computation that lasts at least 2 seconds after the initial time
   given in time0 *)
let rec long_computation time0 =
  let rec generate_list n =
    let rec aux acc = function
      | 0 -> acc
      | n -> aux (float n :: acc) (n-1)
    in
    aux [] n
  in

  let long_list = generate_list 100000 in
  let res = List.length (List.rev_map sin long_list) in
  if Sys.time () -. time0 > 2. then
    Printf.printf "Long computation result: %d\n%!" res
  else long_computation time0


let thread n =
  try long_computation (Sys.time ())
  with Exc -> Printf.printf "Signal handled in thread %d\n%!" n

(* The handler of the signal *)
let interrupt signal =
  raise Exc

let _ =
  (* Block the signal in all threads, except in the main thread. *)
  ignore (Thread.sigmask Unix.SIG_BLOCK [Sys.sigalrm]);

  (* Spawn the threads *)
  ignore (Thread.create thread 1);
  ignore (Thread.create thread 2);
  ignore (Thread.create thread 3);

  (* Unblock the signal in the main thread. *)
  ignore (Thread.sigmask Unix.SIG_UNBLOCK [Sys.sigalrm]);

  (* Setup the alarm *)
  ignore (Unix.alarm 1);
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle interrupt);

  (* Make sure the main thread does something *)
  thread 0
