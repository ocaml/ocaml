(* TEST
   native-compiler;
   setup-ocamlopt.byte-build-env;
   set BUILD_PATH_PREFIX_MAP="app/foo=${test_build_directory}";
   (* Make the program's relative path absolute: *)
   all_modules = "${test_build_directory}/build_path_prefix_map.ml";
   ocamlopt.byte;
   run;
   check-program-output;
*)

let f = fun ?(call_pos = [%call_pos]) () -> call_pos
let _ = print_endline (Textloc.filename (f ()));;
