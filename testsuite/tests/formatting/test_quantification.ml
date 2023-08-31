(* TEST
 compile_only = "true";
 {
   setup-ocamlc.byte-build-env;
   flags = "-g -dno-unique-ids -dno-locations -dtypedtree";
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/test_quantification.reference";
   check-ocamlc.byte-output;
 }
*)

let x = [] in x;;

match [] with x -> x;;

let f x = let y = x,[] in y;;

class c = let x = [] in object method private x = x end;;
