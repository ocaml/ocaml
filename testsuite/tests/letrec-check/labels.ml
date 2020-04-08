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
  let rec x = f ~x;;
              ^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;
