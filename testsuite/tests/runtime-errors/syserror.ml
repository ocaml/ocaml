(* TEST
 flags = "-w -a";
 if target-windows
 then reference = "${test_source_directory}/syserror.win32.reference"
 else reference = "${test_source_directory}/syserror.unix.reference";
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   exit_status = "2";
   run;
   hasunix;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   exit_status = "2";
   run;
   hasunix;
   check-program-output;
 }
*)

let _ = Printexc.record_backtrace false

let channel = open_out "titi:/toto"
