(* TEST
   * expect
*)

type _ ty = Int : int ty
type dyn = Dyn : 'a ty * 'a -> dyn
[%%expect{|
type _ ty = Int : int ty
type dyn = Dyn : 'a ty * 'a -> dyn
|}]

(* Simple use of local variable *)
let f = function <'a> Dyn (w,(x:'a)) -> 3
[%%expect{|
val f : dyn -> int = <fun>
|}]

(* May shadow a global type variable *)
let f : 'a -> int =
  function <'a> Dyn (w,(x:'a)) -> let Int : 'a ty = w in x
[%%expect{|
val f : dyn -> int = <fun>
|}]

(* Global type variables fail *)
let f = function Dyn (w,(x:'a)) -> 3
[%%expect{|
Line 1, characters 24-30:
1 | let f = function Dyn (w,(x:'a)) -> 3
                            ^^^^^^
Error: This pattern matches values of type 'a
       but a pattern was expected which matches values of type $Dyn_'a
       The type constructor $Dyn_'a would escape its scope
|}]
