(* TEST
   * no-flat-float-array
   ** expect
*)

let rec x = [| x |]; 1.;;
[%%expect{|
Line 1, characters 12-19:
1 | let rec x = [| x |]; 1.;;
                ^^^^^^^
Warning 10 [non-unit-statement]: this expression should have type unit.
val x : float = 1.
|}];;

let rec x = let u = [|y|] in 10. and y = 1.;;
[%%expect{|
Line 1, characters 16-17:
1 | let rec x = let u = [|y|] in 10. and y = 1.;;
                    ^
Warning 26 [unused-var]: unused variable u.
val x : float = 10.
val y : float = 1.
|}];;
