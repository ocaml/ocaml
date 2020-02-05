(* TEST
   * expect
*)

(* Test a success case *)
#use_output {|echo let x = 42|}
[%%expect {|
val x : int = 42
|}];;

(* When the command fails *)
#use_output {|false|}
[%%expect {|
Command exited with code 1.
|}];;

(* When the code is invalid *)
#use_output {|echo 1 :: x|}
[%%expect {|
File "(command-output)", line 1, characters 5-6:
1 | 1 :: x
         ^
Error: This expression has type int but an expression was expected of type
         int list
|}];;
