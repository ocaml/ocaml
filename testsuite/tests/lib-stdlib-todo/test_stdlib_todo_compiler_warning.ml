(* TEST
  expect;
*)

[@@@ocaml.alert "++todo"];;

Stdlib.todo ();;
[%%expect {|
Line 3, characters 0-11:
3 | Stdlib.todo ();;
    ^^^^^^^^^^^
Error (alert todo): Stdlib.todo
TODO: not implemented yet
|}];;
