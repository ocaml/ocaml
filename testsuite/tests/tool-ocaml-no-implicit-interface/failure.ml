(* TEST

all_modules = "failure.ml"
flags = "-no-implicit-interface"
compiler_output = "compiler-output.raw"

ocamlc_byte_exit_status = "2"
ocamlopt_byte_exit_status = "2"

* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output

* setup-ocamlopt.byte-build-env
** ocamlopt.byte
*** check-ocamlopt.byte-output
*)

(* Check that error is raised when compiling ml file with no mli file. *)
type t = int
let x = 3
