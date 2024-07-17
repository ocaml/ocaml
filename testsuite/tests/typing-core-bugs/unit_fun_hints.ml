(* TEST
 flags = "-strict-sequence";
 expect;
*)

let g f = f ()
let _ = g 3;;       (* missing `fun () ->' *)

[%%expect{|
val g : (unit -> 'a) -> 'a = <fun>
Line 2, characters 10-11:
2 | let _ = g 3;;       (* missing `fun () ->' *)
              ^
Error: The constant "3" has type "int" but an expression was expected of type
         "unit -> 'a"
       Hint: Did you forget to wrap the expression using "fun () ->"?
|}];;


let _ =
   print_int 3;
   print_newline;    (* missing unit argument *)
   print_int 5;;

(* We use -strict-sequence for this test: otherwise only a warning is produced
   about print_newline not being of type unit *)
[%%expect{|
Line 3, characters 3-16:
3 |    print_newline;    (* missing unit argument *)
       ^^^^^^^^^^^^^
Error: The value "print_newline" has type "unit -> unit"
       but an expression was expected of type "unit"
       because it is in the left-hand side of a sequence
       Hint: Did you forget to provide "()" as argument?
|}];;

let x = read_int in   (* missing unit argument *)
print_int x;;

[%%expect{|
Line 2, characters 10-11:
2 | print_int x;;
              ^
Error: The value "x" has type "unit -> int"
       but an expression was expected of type "int"
       Hint: Did you forget to provide "()" as argument?
|}];;

let g f =
  let _ = f () in
  f = 3;;

[%%expect{|
Line 3, characters 6-7:
3 |   f = 3;;
          ^
Error: The constant "3" has type "int" but an expression was expected of type
         "unit -> 'a"
       Hint: Did you forget to wrap the expression using "fun () ->"?
|}];;

let g f =
  let _ = f () in
  3 = f;;

[%%expect{|
Line 3, characters 6-7:
3 |   3 = f;;
          ^
Error: The value "f" has type "unit -> 'a" but an expression was expected of type
         "int"
       Hint: Did you forget to provide "()" as argument?
|}]
