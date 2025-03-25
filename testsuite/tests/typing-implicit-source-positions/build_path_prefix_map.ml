(* TEST
   native-compiler;
   setup-ocamlopt.byte-build-env;
   set build_path_prefix = "${test_build_directory}";
   (* Escape special characters in the prefix path as per
      BUILD_PATH_PREFIX_MAP's specification *)
   script =
     "sh ${test_source_directory}/encode_build_path_prefix_map.sh \
      ${build_path_prefix}";
   script;
   set BUILD_PATH_PREFIX_MAP="app/foo=${build_path_prefix}";
   (* Make the program's relative path absolute: *)
   all_modules = "${test_build_directory}/build_path_prefix_map.ml";
   ocamlopt.byte;
   run;
   check-program-output;
*)

let f = fun ?(call_pos = [%call_pos]) () -> call_pos
let _ = print_endline (Textloc.filename (f ()));;
