(* TEST
 ocamldebug_script = "${test_source_directory}/input_script";
 {
   dumpenv;
 }{
   readonly_files = "input_script_in";
   debugger;

   shared-libraries;

   setup-ocamlc.byte-build-env;

   script = "mkdir out";
   script;

   flags = "-g -c";
   set BUILD_PATH_PREFIX_MAP = "/stdlib_root=${bppm_encode ${ocamlsrcdir}}/stdlib";
   BUILD_PATH_PREFIX_MAP += ":/source_root=${bppm_encode ${test_source_directory}}";
   BUILD_PATH_PREFIX_MAP += ":/build_root=${bppm_encode ${test_build_directory}}";
   all_modules = "${test_source_directory}/in/blah.ml";
   program = "out/blah.cmo";
   ocamlc.byte;

   program = "out/foo.cmo";
   flags = "-I out -g -c";
   all_modules = "${test_source_directory}/in/foo.ml";
   ocamlc.byte;

   all_modules = "out/blah.cmo out/foo.cmo";
   flags = " -g ";
   program = "debuggee.exe";
   ocamlc.byte;

   check-ocamlc.byte-output;

   set the_map = "${bppm_encode ${ocamlsrcdir}}/stdlib=/stdlib_root";
   the_map += ":${bppm_encode ${test_source_directory}}=/source_root";
   the_map += ":${bppm_encode ${test_build_directory}}=/build_root";
   src = "input_script_in";
   dst = "input_script";
   expand;

   ocamldebug_script = "${test_build_directory}/input_script";
   ocamldebug;

   check-program-output;
 }
*)

(* This file only contains the specification of how to run the test *)
