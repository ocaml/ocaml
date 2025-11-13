let add x y = x + y

let multiply x y = x * y

let main () =
  let a = add 10 20 in
  let b = multiply 5 6 in
  print_int (a + b);
  print_newline ()
