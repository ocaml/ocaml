(* TEST
   flags = "-w +A-70"
   * expect
*)

let rec fact = function
  | 1 -> 1
  | n -> n * (fact [@tailcall]) (n-1)
;;
[%%expect {|
Line 3, characters 13-37:
3 |   | n -> n * (fact [@tailcall]) (n-1)
                 ^^^^^^^^^^^^^^^^^^^^^^^^
Warning 51 [wrong-tailcall-expectation]: expected tailcall
val fact : int -> int = <fun>
|}]

let rec fact = function
  | 1 -> 1
  | n -> n * (fact [@tailcall true]) (n-1)
;;
[%%expect {|
Line 3, characters 13-42:
3 |   | n -> n * (fact [@tailcall true]) (n-1)
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 51 [wrong-tailcall-expectation]: expected tailcall
val fact : int -> int = <fun>
|}]

let rec fact = function
  | 1 -> 1
  | n -> n * (fact [@tailcall false]) (n-1)
;;
[%%expect {|
val fact : int -> int = <fun>
|}]

let rec fact_tail acc = function
  | 1 -> acc
  | n -> (fact_tail [@tailcall]) (n * acc) (n - 1)
;;
[%%expect{|
val fact_tail : int -> int -> int = <fun>
|}]

let rec fact_tail acc = function
  | 1 -> acc
  | n -> (fact_tail [@tailcall true]) (n * acc) (n - 1)
;;
[%%expect{|
val fact_tail : int -> int -> int = <fun>
|}]

let rec fact_tail acc = function
  | 1 -> acc
  | n -> (fact_tail [@tailcall false]) (n * acc) (n - 1)
;;
[%%expect{|
Line 3, characters 9-56:
3 |   | n -> (fact_tail [@tailcall false]) (n * acc) (n - 1)
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 51 [wrong-tailcall-expectation]: expected non-tailcall
val fact_tail : int -> int -> int = <fun>
|}]


(* explicitly test the "invalid payload" case *)
let rec test x = (test[@tailcall foobar]) x;;
[%%expect{|
Line 1, characters 24-32:
1 | let rec test x = (test[@tailcall foobar]) x;;
                            ^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'tailcall'.
Only an optional boolean literal is supported.
val test : 'a -> 'b = <fun>
|}]
