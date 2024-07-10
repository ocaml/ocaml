(* TEST

flags = "-bin-annot";
compile_only = "true";
setup-ocamlc.byte-build-env;
readonly_files = "intf_uids.mli intf_uids.ml";
all_modules = "intf_uids.mli intf_uids.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -decls intf_uids.cmti intf_uids.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)

(* This test illustrates the fact that uids, right now, are not unique accross a
compilation unit if we consider a unit being formed of an interface file and an
implementation file. Numbering of uids start at 0 for both, leading to identical uids not pointing to the same declaration *)

type u (* has uid Intf_uids.0 *)

type t (* has uid Intf_uids.1 *)
