(* TEST
   * expect
*)

let f ~x () = x ();;
[%%expect{|
val f : x:(unit -> 'a) -> unit -> 'a = <fun>
|}];;

let rec x = f ~x;;
[%%expect{|
Line 1, characters 12-16:
1 | let rec x = f ~x;;
                ^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let f x ~y = x + y
(* this function creates "abstracted arguments" in the sense of
   Rec_check.is_abstracted_arg. Those should be treated as
   returned/unguarded, and not delayed, otherwise the code below
   segfaults. *)
let rec g = f ~y:(print_endline !y; 0)
and y =
  let _ = g in (* ignore g to have a real dependency *)
  ref "foo";;
[%%expect {|
val f : int -> y:int -> int = <fun>
Line 6, characters 12-38:
6 | let rec g = f ~y:(print_endline !y; 0)
                ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}]
