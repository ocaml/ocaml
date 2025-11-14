(* TEST
   native-compiler;
   readonly_files = "multi_obj_a.ml multi_obj_b.ml";
   script = "sh ${test_source_directory}/multi_obj_dwarf_test.sh";
   script;
*)

(* Main program for multi-object linking test *)
let () =
  let a = Multi_obj_a.add 10 5 in
  let b = Multi_obj_b.subtract 10 5 in
  Printf.printf "add: %d, subtract: %d\n" a b
