(* TEST
* shared-libraries
** setup-ocamlopt.byte-build-env
*** ocamlopt.byte
flags = "-shared"
all_modules = "question.ml"
program = "question.cmxs"
**** check-ocamlopt.byte-output
***** ocamlobjinfo
****** check-program-output
*)

let answer = 42
