(* Test program for DWARF structure validation *)

let rec factorial n =
  if n <= 1 then 1 else n * factorial (n - 1)

let test_function x y =
  let z = x + y in
  factorial z

let () =
  let result = test_function 3 2 in
  Printf.printf "Result: %d\n" result
