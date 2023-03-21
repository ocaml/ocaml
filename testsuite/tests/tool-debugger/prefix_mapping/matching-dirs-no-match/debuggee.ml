(* Test the case where none of the matching directories exist. *)

(* TEST
 flags += " -g ";
 ocamldebug_script = "${test_source_directory}/input_script";
 readonly_files = "printer.ml";
 include debugger;
 include ocamlcommon;
 set DEPLOY_PATH_PREFIX_MAP = "abc=/workspace_root:def=/workspace_root";
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
