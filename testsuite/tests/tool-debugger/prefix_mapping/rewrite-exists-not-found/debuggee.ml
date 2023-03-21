(* Test rewrite_find_first_existing for the case where DEPLOY_PATH_PREFIX_MAP is set,
   there is a prefix match, but the desired file is not found. *)

(* TEST
 flags += " -g ";
 ocamldebug_script = "${test_source_directory}/input_script";
 readonly_files = "printer.ml";
 subdirectories = "abc def";
 include debugger;
 include ocamlcommon;
 set DEPLOY_PATH_PREFIX_MAP = "abc=/workspace_root";
 DEPLOY_PATH_PREFIX_MAP += ":def=/workspace_root";
 debugger;

 shared-libraries;

 setup-ocamlc.byte-build-env;
 {
   module = "printer.ml";
   ocamlc.byte;
 }{
   ocamlc.byte;

   check-ocamlc.byte-output;

   ocamldebug;

   check-program-output;
 }
*)

let f x =
  for _i = 0 to x do
    print_endline "..."
  done

let main () =
  f 3
let () =
  main ()
