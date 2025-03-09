(* TEST

flags = "-bin-annot";
compile_only = "true";
modules = "intf_uids.ml";

setup-ocamlc.byte-build-env;
all_modules = "intf_uids.mli intf_uids.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -decls intf_uids.cmti intf_uids.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)

(* This test illustrates the fact that uids are tagged to indicate if they
  originate from an interface of an implementation: it prints the delcarations
  written in the cmt file for the interface and then for the implementation.
  These should not overlap.  *)
