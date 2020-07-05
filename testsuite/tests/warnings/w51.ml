(* TEST
   flags = "-w A"
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
