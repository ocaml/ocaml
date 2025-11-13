(* TEST
   native-compiler;
   linux;
   arch_amd64;
   script = "sh ${test_source_directory}/../../native-debugger/has_lldb.sh linux";
   script;
   readonly_files = "breakpoint.ml";
   setup-ocamlopt.byte-build-env;
   program = "${test_build_directory}/breakpoint";
   flags = "-g";
   all_modules = "breakpoint.ml";
   ocamlopt.byte;
   debugger_script = "${test_source_directory}/lldb_line_script";
   lldb;
   script = "sh ${test_source_directory}/../../native-debugger/sanitize.sh dwarf_line_lldb_linux";
   script;
   check-program-output;
*)
