(* TEST
include tool-ocaml-lib
flags = "-w -a"
ocaml_script_as_argument = "true"
* setup-ocaml-build-env
** ocaml
*)

type t = {
  mutable a : int;
  mutable b : int;
};;

{ a = 0; b = 0 };;

(**
       0 CONST0
       1 PUSHCONST0
       2 MAKEBLOCK2 0
       4 ATOM0
       5 SETGLOBAL T040-makeblock2
       7 STOP
**)
