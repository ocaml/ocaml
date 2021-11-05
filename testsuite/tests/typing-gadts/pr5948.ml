(* TEST
   * expect
*)

type tag = [`TagA | `TagB | `TagC];;

type 'a poly =
    AandBTags : [< `TagA of int | `TagB ] poly
  | ATag : [< `TagA of int] poly
(* constraint 'a = [< `TagA of int | `TagB] *)
;;

let intA = function `TagA i -> i
let intB = function `TagB -> 4
;;

let intAorB = function
    `TagA i -> i
  | `TagB -> 4
;;

type _ wrapPoly =
    WrapPoly : 'a poly -> ([< `TagA of int | `TagB] as 'a) wrapPoly
;;

let example6 : type a. a wrapPoly -> (a -> int) =
  fun w  ->
    match w with
    | WrapPoly ATag -> intA
    | WrapPoly _ -> intA (* This should not be allowed *)
;;
[%%expect{|
type tag = [ `TagA | `TagB | `TagC ]
type 'a poly =
    AandBTags : [< `TagA of int | `TagB ] poly
  | ATag : [< `TagA of int ] poly
val intA : [< `TagA of 'a ] -> 'a = <fun>
val intB : [< `TagB ] -> int = <fun>
val intAorB : [< `TagA of int | `TagB ] -> int = <fun>
type _ wrapPoly =
    WrapPoly : 'a poly -> ([< `TagA of int | `TagB ] as 'a) wrapPoly
Line 25, characters 23-27:
25 |     | WrapPoly ATag -> intA
                            ^^^^
Error: This expression has type [< `TagA of 'a ] -> 'a
       but an expression was expected of type a -> int
       Type [< `TagA of 'a ] is not compatible with type
         a = [< `TagA of int | `TagB ]
       The first variant type does not allow tag(s) `TagB
|}];;

let _ =  example6 (WrapPoly AandBTags) `TagB (* This causes a seg fault *)
;;
[%%expect{|
Line 1, characters 9-17:
1 | let _ =  example6 (WrapPoly AandBTags) `TagB (* This causes a seg fault *)
             ^^^^^^^^
Error: Unbound value example6
|}];;
