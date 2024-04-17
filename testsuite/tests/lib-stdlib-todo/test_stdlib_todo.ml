(* TEST
 expect;
*)

Stdlib.todo ~__LOC__ ();;
[%%expect {|
Line 1, characters 0-11:
1 | Stdlib.todo ~__LOC__ ();;
    ^^^^^^^^^^^
Alert todo: Stdlib.todo
TODO: not implemented yet

Exception:
Failure "File \"\", line 1, characters 13-20, TODO: not implemented yet".
|}];;

Stdlib.todo ();;
[%%expect {|
Line 1, characters 0-11:
1 | Stdlib.todo ();;
    ^^^^^^^^^^^
Alert todo: Stdlib.todo
TODO: not implemented yet

Exception: Failure "TODO: not implemented yet".
|}];;

Stdlib.todo ~msg:"Im here" ~__LOC__ ();;
[%%expect {|
Line 1, characters 0-11:
1 | Stdlib.todo ~msg:"Im here" ~__LOC__ ();;
    ^^^^^^^^^^^
Alert todo: Stdlib.todo
TODO: not implemented yet

Exception: Failure "File \"\", line 1, characters 28-35, TODO: Im here".
|}];;


Stdlib.todo ~msg:"Im here" ();;
[%%expect {|
Line 1, characters 0-11:
1 | Stdlib.todo ~msg:"Im here" ();;
    ^^^^^^^^^^^
Alert todo: Stdlib.todo
TODO: not implemented yet

Exception: Failure "TODO: Im here".
|}];;
