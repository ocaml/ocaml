(* TEST_BELOW
(* Blank lines added here to preserve locations. *)












*)

module A' = A (* missing a.cmi *)
module B' = B (* broken b.cmi *)
module C' = C (* valid c.cmi *)
module D' = D (* valid d.cmi *)
let () = print_int D'.something

(* TEST
 flags = "-no-alias-deps";
 compile_only = "true";
 readonly_files = "b.cmi.in c.mli d.mli";
 setup-ocamlc.byte-build-env;
 src = "b.cmi.in";
 dst = "b.cmi";
 copy;
 all_modules = "c.mli d.mli aliases.ml";
 ocamlc.byte;
 check-ocamlc.byte-output;
 program = "aliases.cmo";
 ocamlobjinfo;
 check-program-output;
*)
