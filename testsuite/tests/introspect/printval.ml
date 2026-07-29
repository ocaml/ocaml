(* TEST
   setup-ocamlopt.opt-build-env;
   script = "sh ${test_source_directory}/has-introspect.sh";
   script;
*)

open Introspect.Print

let () = print_any_endline 1
let () = print_any_endline "Hello"
let () = print_any_endline [1;2;3]
let () = print_any_endline (let rec l = 1 :: l in l)

module M = Map.Make(Int)
let () = print_any_endline (M.of_list [1, 1; 2, 2; 3, 3])
