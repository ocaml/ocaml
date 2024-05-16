(* TEST
  flags="-alert ++todo";
  expect;
*)


Stdlib.todo ();;
[%%expect {|
Line 1, characters 0-11:
1 | Stdlib.todo ();;
    ^^^^^^^^^^^
Error (alert todo): Stdlib.todo
TODO: not implemented yet
|}];;
