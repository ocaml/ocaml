(* TEST
  * expect
*)

		let x = abc
;;
[%%expect{|
Line 1, characters 10-13:
1 | 		let x = abc
    		        ^^^
Error: Unbound value abc
Hint: Did you mean abs?
|}];;
