(* TEST
   * expect
*)

type (_,_) eq = Refl : ('a,'a) eq;;
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
|}]

(* Both should fail *)
let f (type a b) (w1 : (a, b -> b) eq) (w2 : (a, int -> int) eq) (g : a) =
   let Refl = w1 in let Refl = w2 in g 3;;
[%%expect{|
val f : ('a, 'b -> 'b) eq -> ('a, int -> int) eq -> 'a -> 'b = <fun>
|}, Principal{|
Line 2, characters 37-40:
2 |    let Refl = w1 in let Refl = w2 in g 3;;
                                         ^^^
Error: This expression has type b = int
       but an expression was expected of type 'a
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}]
let f (type a b) (w1 : (a, b -> b) eq) (w2 : (a, int -> int) eq) (g : a) =
   let Refl = w2 in let Refl = w1 in g 3;;
[%%expect{|
val f : ('a, 'b -> 'b) eq -> ('a, int -> int) eq -> 'a -> int = <fun>
|}, Principal{|
Line 2, characters 37-40:
2 |    let Refl = w2 in let Refl = w1 in g 3;;
                                         ^^^
Error: This expression has type int but an expression was expected of type 'a
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}]

(* Ok *)
let f (type a b) (w1 : (a, b -> b) eq) (w2 : (a, int -> int) eq) (g : a) : b =
   let Refl = w2 in let Refl = w1 in g 3;;
[%%expect{|
val f : ('a, 'b -> 'b) eq -> ('a, int -> int) eq -> 'a -> 'b = <fun>
|}]
