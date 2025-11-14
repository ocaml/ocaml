let test_variables () =
  let int_var = 42 in
  let float_var = 3.14 in
  let string_var = "hello" in
  let bool_var = true in
  let tuple_var = (1, 2) in
  Printf.printf "int=%d float=%f string=%s bool=%b\n"
    int_var float_var string_var bool_var;
  int_var + fst tuple_var

let () =
  let result = test_variables () in
  Printf.printf "result=%d\n" result
