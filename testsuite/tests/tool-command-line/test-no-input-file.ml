(* TEST
 setup-ocamlopt.opt-build-env;
 all_modules = "";
 compile_only = "true";
 ocamlopt_opt_exit_status = "2";
 flags = "";
 ocamlopt.opt;
 flags = "-o test.exe";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(*
  This file is just a test driver, the test does not contain any
  real OCaml code
 *)
