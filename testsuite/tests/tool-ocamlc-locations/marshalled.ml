(* TEST
   readonly_files="foo.ml";
   setup-ocamlc.byte-build-env;
   {
   include ocamlcommon;
   program = "marshalled.byte";
   all_modules = "marshalled.ml";
   ocamlc.byte;
   script = "./marshalled.byte";
   script;
   }
   {
   all_modules = "foo.marshalled.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   check-ocamlc.byte-output;
   }
*)

(* This is a repro case for #12697 *)

let () =
  let ast = Pparse.parse_implementation ~tool_name:"test" "foo.ml" in
  Pparse.write_ast Pparse.Structure "foo.marshalled.ml" ast
