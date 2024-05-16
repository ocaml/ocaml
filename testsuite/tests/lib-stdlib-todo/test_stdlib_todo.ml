(* TEST
 expect;
*)


Printexc.record_backtrace true;;

Stdlib.todo ();;
[%%expect {|
- : unit = ()
Line 3, characters 0-11:
3 | Stdlib.todo ();;
    ^^^^^^^^^^^
Alert todo: Stdlib.todo
TODO: not implemented yet

Exception: Failure "TODO: not implemented yet".

Called from unknown location
Called from unknown location
|}];;


Stdlib.todo ~msg:"Im here" ();;
[%%expect {|
Line 1, characters 0-11:
1 | Stdlib.todo ~msg:"Im here" ();;
    ^^^^^^^^^^^
Alert todo: Stdlib.todo
TODO: not implemented yet

Exception: Failure "TODO: Im here".

Called from unknown location
Called from unknown location
|}];;

Printexc.record_backtrace false;;

[@@@ocaml.alert "-todo"];;

Stdlib.todo ();;
[%%expect {|
- : unit = ()
Exception: Failure "TODO: not implemented yet".
|}];;


Stdlib.todo ~msg:"Im here" ();;
[%%expect {|
Exception: Failure "TODO: Im here".
|}];;
