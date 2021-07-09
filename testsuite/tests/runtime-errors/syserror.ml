(* TEST

flags = "-w -a"

* setup-ocamlc.byte-build-env
** ocamlc.byte
*** run
exit_status = "2"
**** libunix
***** check-program-output
reference = "${test_source_directory}/syserror.unix.reference"
**** libwin32unix
***** check-program-output
reference = "${test_source_directory}/syserror.win32.reference"

* setup-ocamlopt.byte-build-env
** ocamlopt.byte
*** run
exit_status = "2"
**** libunix
***** check-program-output
reference = "${test_source_directory}/syserror.unix.reference"
**** libwin32unix
***** check-program-output
reference = "${test_source_directory}/syserror.win32.reference"

*)

let _ = Printexc.record_backtrace false

let channel = open_out "titi:/toto"
