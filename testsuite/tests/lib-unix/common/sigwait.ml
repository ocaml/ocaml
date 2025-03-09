(* TEST
 include unix;
 libunix;
 {
   bytecode;
 }{
   native;
 }
*)

let handler _signo =
  print_string "Should not happen!"; print_newline()

let raiser () =
  Unix.kill (Unix.getpid()) Sys.sigusr1

let _ =
  Sys.set_signal Sys.sigusr1 (Sys.Signal_handle handler);
  let signals_of_interest = [Sys.sigusr1; Sys.sigusr2] in
  ignore (Unix.(sigprocmask SIG_BLOCK signals_of_interest));
  let d = Domain.spawn raiser in
  let signo = Unix.sigwait signals_of_interest in
  Domain.join d;
  assert (signo = Sys.sigusr1)
