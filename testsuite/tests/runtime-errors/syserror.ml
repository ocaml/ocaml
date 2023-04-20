(* TEST
 flags = "-w -a";
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   exit_status = "2";
   run;
   {
     libunix;
     reference = "${test_source_directory}/syserror.unix.reference";
     check-program-output;
   }{
     libwin32unix;
     reference = "${test_source_directory}/syserror.win32.reference";
     check-program-output;
   }
 }{
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   exit_status = "2";
   run;
   {
     libunix;
     reference = "${test_source_directory}/syserror.unix.reference";
     check-program-output;
   }{
     libwin32unix;
     reference = "${test_source_directory}/syserror.win32.reference";
     check-program-output;
   }
 }
*)

let _ = Printexc.record_backtrace false

let channel = open_out "titi:/toto"
