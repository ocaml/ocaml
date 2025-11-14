(* Main program for multi-object linking test *)
let () =
  let a = Multi_obj_a.add 10 5 in
  let b = Multi_obj_b.subtract 10 5 in
  Printf.printf "add: %d, subtract: %d\n" a b
