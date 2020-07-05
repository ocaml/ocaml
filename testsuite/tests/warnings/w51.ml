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
Warning 51 [tailcall-expected]: expected tailcall
val fact : int -> int = <fun>
|}]
