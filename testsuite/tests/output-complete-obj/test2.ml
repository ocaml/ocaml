(* TEST

readonly_files = "puts.c"
use_runtime = "false"
unset FOO

* hasunix
include unix
** setup-ocamlc.byte-build-env
*** ocamlc.byte
flags = "-w -a -output-complete-exe puts.c -ccopt -I${ocamlsrcdir}/runtime"
program = "test2"
**** run
program = "./test2"
***** check-program-output
*)

external puts: string -> unit = "caml_puts"

let _ = at_exit (fun () -> print_endline "Program terminated")

let () =
  Unix.putenv "FOO" "Hello OCaml!";
  puts (Unix.getenv "FOO")
