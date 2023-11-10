(* TEST
 flags = "-w +A-70";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Warning 47 triggers on too few arguments *)
let x1 = 42 [@@ppwarning]

(* Warning 47 triggers on too many arguments *)
let x2 = 42 [@@ppwarning "foo" "bar"]

(* Warning 47 triggers on the wrong sort of argument *)
let x3 = 42 [@@ppwarning 84]

(* Warning 47 doesn't trigger on a single string argument *)
let x4 = 42 [@@ppwarning "foo"]
