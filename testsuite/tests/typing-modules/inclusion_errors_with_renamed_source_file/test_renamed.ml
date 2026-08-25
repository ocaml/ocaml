(* TEST
 readonly_files = "foo.ml foo.mli parse_and_marshall.ml";
 setup-ocamlc.byte-build-env;

 (* Build parse_and_marshall.exe, which is used to create a serialized AST. *)
 include ocamlcommon;
 program = "${test_build_directory}/parse_and_marshall.exe";
 all_modules = "parse_and_marshall.ml";
 ocamlc.byte;

 (* Run parse_and_marshall.exe to create a serialized AST. *)
 script = "${program} foo.ml foo.pp.ml";
 script;

 (* Now delete foo.ml. In a real-world scenario, the original source file may
    no longer exist (for example, due to dune sandboxing). *)
 script = "rm foo.ml";
 script;

 (* Compile the .mli file. *)
 module = "foo.mli";
 ocamlc.byte;

 (* Feed the compiler the serialzed AST. We also pass -bin-annot and to verify that .cmt
    generation works as expected when the original source file no longer exists. *)
 module = "-impl foo.pp.ml";
 flags = "-cmi-file foo.cmi -bin-annot";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* This test checks that when the compiler is fed a serialized AST rather than a .ml file,
   it reports the name of the original file rather than the name of the serialized file in
   error messages. This can happen if Dune runs the ppx stage separately from compilation
   and feeds the compiler the ppx-expanded AST. In this case, reporting the original .ml
   file is a better user experience. *)
