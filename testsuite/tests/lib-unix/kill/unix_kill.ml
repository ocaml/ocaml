(* TEST
include unix
* libunix
** bytecode
** native
*)

let () =
  let r = ref false in
  Sys.set_signal Sys.sigint (Signal_handle (fun _ -> r := true));
  Unix.kill (Unix.getpid ()) Sys.sigint;
  let x = !r in
  Printf.printf "%b " x;
  Printf.printf "%b\n" !r

let () =
  let r = ref false in
  let _ = Unix.sigprocmask SIG_BLOCK [Sys.sigint] in
  Sys.set_signal Sys.sigint (Signal_handle (fun _ -> r := true));
  Unix.kill (Unix.getpid ()) Sys.sigint;
  Gc.full_major ();
  let a = !r in
  let _ = Unix.sigprocmask SIG_UNBLOCK [Sys.sigint] in
  let b = !r in
  Printf.printf "%b %b " a b;
  Printf.printf "%b\n" !r
