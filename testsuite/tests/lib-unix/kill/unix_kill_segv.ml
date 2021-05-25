(* TEST
   include unix
   * libunix
   ** not-macos
   *** bytecode
   *** native

  (* Test fails on MacOS, see runtime/signals_nat.c. *)
*)

let () =
  match Unix.fork () with
  | 0 -> Unix.kill (Unix.getpid ()) Sys.sigsegv
  | pid -> (
      if Unix.wait () <> (pid, Unix.WSIGNALED Sys.sigsegv)
      then failwith "Survived SIGSEGV"
    )
