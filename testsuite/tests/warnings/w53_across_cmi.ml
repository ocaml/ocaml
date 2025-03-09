(* TEST
   readonly_files = "w53_without_cmi.ml w53_with_cmi.mli w53_with_cmi.ml";
   setup-ocamlc.byte-build-env;
   all_modules = "w53_without_cmi.ml w53_with_cmi.mli w53_with_cmi.ml";
   flags = "-w +A-70";
   compile_only = "true";
   ocamlc.byte;
   all_modules = "w53_across_cmi.ml";
   flags = "-alert +all -w +A-33-70";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

(* This tests checks that alerts are correctly triggered across compilation
   units. *)

open W53_without_cmi
open W53_with_cmi
