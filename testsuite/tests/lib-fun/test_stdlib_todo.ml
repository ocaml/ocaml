(* TEST
 expect;
*)


Printexc.record_backtrace true;;

Fun.todo ();;
[%%expect {|
- : unit = ()
Exception: Fun.Todo
File "-", line 3
Called from unknown location
|}];;


Printexc.record_backtrace false;;

[@@@ocaml.alert "+todo"];;

Fun.todo ();;
[%%expect {|
- : unit = ()
Line 5, characters 0-8:
5 | Fun.todo ();;
    ^^^^^^^^
Alert todo: Stdlib.Fun.todo
Unimplemented functionality, may lead to runtime errors

Exception: Fun.Todo
File "-", line 5
|}];;

[@@@ocaml.alert "++todo"];;

Fun.todo ();;
[%%expect {|
Line 3, characters 0-8:
3 | Fun.todo ();;
    ^^^^^^^^
Error (alert todo): Stdlib.Fun.todo
Unimplemented functionality, may lead to runtime errors
|}];;
