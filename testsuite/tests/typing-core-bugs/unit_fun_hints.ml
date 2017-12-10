let g f = f ()
let _ = g 3;;       (* missing `fun () ->' *)

[%%expect{|
val g : (unit -> 'a) -> 'a = <fun>
Line _, characters 10-11:
Error: This expression has type int but an expression was expected of type
         unit -> 'a
       Hint: Did you forget to wrap the expression using `fun () ->'?
|}];;


let _ =
   print_int 3;
   print_newline;    (* missing unit argument *)
   print_int 5;;

(* We use -strict-sequence for this test: otherwise only a warning is produced
   about print_newline not being of type unit *)
[%%expect{|
Line _, characters 3-16:
Error: This expression has type unit -> unit
       but an expression was expected of type unit
       Hint: Did you forget to provide `()' as argument?
|}];;

let x = read_int in   (* missing unit argument *)
print_int x;;

[%%expect{|
Line _, characters 10-11:
Error: This expression has type unit -> int
       but an expression was expected of type int
       Hint: Did you forget to provide `()' as argument?
|}];;

let g f =
  let _ = f () in
  f = 3;;

[%%expect{|
Line _, characters 6-7:
Error: This expression has type int but an expression was expected of type
         unit -> 'a
       Hint: Did you forget to wrap the expression using `fun () ->'?
|}];;

let g f =
  let _ = f () in
  3 = f;;

[%%expect{|
Line _, characters 6-7:
Error: This expression has type unit -> 'a
       but an expression was expected of type int
       Hint: Did you forget to provide `()' as argument?
|}]
