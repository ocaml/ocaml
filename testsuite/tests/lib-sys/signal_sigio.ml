(* TEST
 include unix;
 hasunix;
 not beos_haiku;
 not windows;
 native;
*)
open Sys

let () =
  let r = ref false in
  Sys.set_signal Sys.sigio (Signal_handle (fun _ -> r := true));
  Unix.kill (Unix.getpid ()) Sys.sigio;
  let x = !r in
  assert (x == true); (* Should trigger signal_handle for sigio *)
