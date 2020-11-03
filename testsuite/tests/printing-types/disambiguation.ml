(* TEST
   * expect
*)

type 'a x = private [> `x] as 'a;;
[%%expect {|
Line 1:
Error: Type declarations do not match:
         type !'a x = private [> `x ] constraint 'a = 'a x
       is not included in
         type 'a x
       Their constraints differ.
|}, Principal{|
Line 1:
Error: Type declarations do not match:
         type !'a x = private 'a constraint 'a = [> `x ]
       is not included in
         type 'a x
       Their constraints differ.
|}];;


type int;;
[%%expect {|
type int
|}];;

let x = 0;;
[%%expect {|
val x : int/2 = 0
|}];;


type float;;
[%%expect {|
type float
|}];;

0.;;
[%%expect {|
- : float/2 = 0.
|}];;
