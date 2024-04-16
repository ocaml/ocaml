(* TEST
 subdirectories = "lib1 lib2";
 readonly_files = "lib1_client.ml lib2_client.ml";
 compile_only = "true";
 setup-ocamlopt.byte-build-env;

 (* Set up the Lib modules that the client modules depend on *)
 all_modules = "lib1/lib.ml";
 ocamlopt.byte;
 all_modules = "lib2/lib.ml";
 ocamlopt.byte;

 (* Compile Lib1_client against Lib1 *)
 flags = "-I lib1";
 all_modules = "lib1_client.ml";
 ocamlopt.byte;

 (* Compile Lib2_client against Lib2 *)
 flags = "-I lib2";
 all_modules = "lib2_client.ml";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* This test is a regression test. The bug was in the last step: the compiler crashed
   with an exception and backtrace instead of printing a useful error message. The
   issue was that the compiler was erroneously running in a mode where its error reporting
   is allowed to load cmi files from disk. This mode is undesirable because it means
   that the compiler can encounter new exceptions (e.g. that the new cmi file it loads
   is not consistent with other cmi files) while doing error reporting for the old
   exception.
 *)
