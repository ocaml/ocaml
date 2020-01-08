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


module M : sig type t end =
		struct
		  type u
		end
;;
[%%expect{|
Lines 2-4, characters 2-5:
2 | 		struct
3 | 		  type u
4 | 		end
Error: Signature mismatch:
       Modules do not match: sig type u end is not included in sig type t end
       The type `t' is required but not provided
|}];;
