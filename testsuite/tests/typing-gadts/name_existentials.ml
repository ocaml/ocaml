(* TEST
   * expect
*)

type _ ty = Int : int ty
type dyn = Dyn : 'a ty * 'a -> dyn
[%%expect{|
type _ ty = Int : int ty
type dyn = Dyn : 'a ty * 'a -> dyn
|}]

let ok1 = function Dyn (type a) (w, x : a ty * a) -> ignore (x : a)
let ok2 = function Dyn (type a) (w, x : _ * a) -> ignore (x : a)
[%%expect{|
val ok1 : dyn -> unit = <fun>
val ok2 : dyn -> unit = <fun>
|}]

let ko1 = function Dyn (type a) (w, x : _) -> ()
[%%expect{|
Line 1, characters 40-41:
1 | let ko1 = function Dyn (type a) (w, x : _) -> ()
                                            ^
Error: This type does not bind all existentials in the constructor:
         type a. 'a ty * 'a
|}]
let ko2 = function Dyn (type a b) (a, x : a ty * b) -> ignore (x : b)
[%%expect{|
Line 1, characters 42-50:
1 | let ko2 = function Dyn (type a b) (a, x : a ty * b) -> ignore (x : b)
                                              ^^^^^^^^
Error: This pattern matches values of type a ty * b
       but a pattern was expected which matches values of type a ty * a
       Type b is not compatible with type a
|}]

type u = C : 'a * ('a -> 'b list) -> u
let f = function C (type a b) (x, f : _ * (a -> b list)) -> ignore (x : a)
[%%expect{|
type u = C : 'a * ('a -> 'b list) -> u
val f : u -> unit = <fun>
|}]

let f = function C (type a) (x, f : a * (a -> a list)) -> ignore (x : a)
[%%expect{|
Line 1, characters 36-53:
1 | let f = function C (type a) (x, f : a * (a -> a list)) -> ignore (x : a)
                                        ^^^^^^^^^^^^^^^^^
Error: This type does not bind all existentials in the constructor:
         type a. a * (a -> a list)
|}]
