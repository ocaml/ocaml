(* TEST
   unset BUILD_PATH_PREFIX_MAP;
   native-compiler;
   no-tsan; (* Skip, TSan inserts extra frames into backtraces *)
   linux;
   not-clang; (* Skip, clang is tested on macOS *)
   arch_arm64;
   script = "sh ${test_source_directory}/has_lldb.sh linux";
   script;
   readonly_files = "meander.ml meander_c.c lldb_test.py";
   setup-ocamlopt.byte-build-env;
   program = "${test_build_directory}/meander";
   flags = "-g -ccopt -O0";
   all_modules = "meander.ml meander_c.c";
   ocamlopt.byte;
   debugger_script = "${test_source_directory}/lldb-script";
   lldb;
   script = "sh ${test_source_directory}/sanitize.sh linux-lldb-arm64";
   script;
   check-program-output;
 *)
