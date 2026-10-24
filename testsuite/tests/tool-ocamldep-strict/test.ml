(* TEST
 readonly_files = "broken.ml cycle_a.ml cycle_b.ml";
 setup-ocamlc.byte-build-env;
 {
   (* Missing input file: silently ignored. *)
   commandline = "-depend -modules test.ml doesnotexist.ml";
   compiler_output = "${test_build_directory}/default.output";
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/default.reference";
   check-ocamlc.byte-output;
 }{
   (* -strict with no errors: dependencies are printed as usual. *)
   commandline = "-depend -modules -strict test.ml";
   compiler_output = "${test_build_directory}/strict_ok.output";
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/strict_ok.reference";
   check-ocamlc.byte-output;
 }{
   (* Missing input file with -strict: error, and no dependencies are
      printed. *)
   commandline = "-depend -modules -strict test.ml doesnotexist.ml";
   compiler_output = "${test_build_directory}/strict_missing.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/strict_missing.reference";
   check-ocamlc.byte-output;
 }{
   (* Syntax error: error. *)
   commandline = "-depend -modules broken.ml";
   compiler_output = "${test_build_directory}/syntax_error.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/syntax_error.reference";
   check-ocamlc.byte-output;
 }{
   (* Syntax error with -strict: error, and no dependencies are printed. *)
   commandline = "-depend -modules -strict test.ml broken.ml";
   compiler_output = "${test_build_directory}/strict_syntax_error.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/strict_syntax_error.reference";
   check-ocamlc.byte-output;
 }{
   (* Dependency cycle with -sort: error, files are printed anyway. *)
   commandline = "-depend -sort test.ml cycle_a.ml cycle_b.ml";
   compiler_output = "${test_build_directory}/sort_cycle.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/sort_cycle.reference";
   check-ocamlc.byte-output;
 }
*)

(* Behaviour of [ocamldep -strict] on missing and unparsable input files,
   and on dependency cycles with [-sort]. *)

let _ = List.length
