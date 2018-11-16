(* TEST
   * expect
*)

(* PR#5907 *)

type 'a t = 'a;;
let f (g : 'a list -> 'a t -> 'a) s = g s s;;
[%%expect{|
type 'a t = 'a
Line 2, characters 42-43:
2 | let f (g : 'a list -> 'a t -> 'a) s = g s s;;
                                              ^
Error: This expression has type 'a list
       but an expression was expected of type 'a t = 'a
       The type variable 'a occurs inside 'a list
|}];;
let f (g : 'a * 'b -> 'a t -> 'a) s = g s s;;
[%%expect{|
Line 1, characters 42-43:
1 | let f (g : 'a * 'b -> 'a t -> 'a) s = g s s;;
                                              ^
Error: This expression has type 'a * 'b
       but an expression was expected of type 'a t = 'a
       The type variable 'a occurs inside 'a * 'b
|}];;
