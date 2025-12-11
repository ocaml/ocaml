(* TEST

flags += " -runtime-variant d";
bytecode;

*)

(* This file tests that we can use -runtime-variant in conjunction with
   -use-runtime.
   When testing in bytecode, ocamltest always uses -use-runtime, and here
   we add -runtime-variant. Comparison with the .reference file will check
   that -runtime-variant is correctly added to the runtime file name.
*)
