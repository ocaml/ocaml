let x = [1;2;3]

let f x = 1 :: 2 :: 3 :: x

let _ =
  if x = f [] then print_string "OK " else print_string "failed! ";
  if stdout <> stderr then print_string "OK" else print_string "failed!";
  print_newline()
