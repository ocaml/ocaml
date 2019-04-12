(* TEST
   * expect
*)

type 'a x = private [> `x] as 'a;;
[%%expect {|
Line 1:
Error: Type declarations do not match:
         type 'a x = private [> `x ] constraint 'a = 'a x/2
       is not included in
         type 'a x
       Their constraints differ.
       File "_none_", line 1:
         Definition of type x/1
       Line 1, characters 0-32:
         Definition of type x/2
|}, Principal{|
Line 1:
Error: Type declarations do not match:
         type 'a x = private 'a constraint 'a = [> `x ]
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
