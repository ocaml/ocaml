(* TEST
 shared-libraries;
 setup-ocamlopt.byte-build-env;
 flags = "-shared";
 all_modules = "question.ml";
 program = "question.cmxs";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 ocamlobjinfo;
 check-program-output;
*)

let answer = 42
