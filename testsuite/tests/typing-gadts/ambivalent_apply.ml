(* TEST
 expect;
*)

type (_,_) eq = Refl : ('a,'a) eq;;
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
|}]

(* Both should fail *)
let f (type a b) (w1 : (a, b -> b) eq) (w2 : (a, int -> int) eq) (g : a) =
   let Refl = w1 in let Refl = w2 in g 3;;
[%%expect{|
Line 2, characters 37-40:
2 |    let Refl = w1 in let Refl = w2 in g 3;;
                                         ^^^
Error: This expression has type "b" = "int"
       but an expression was expected of type "'a"
       This instance of "int" is ambiguous:
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
Error: This expression has type "int" but an expression was expected of type "'a"
       This instance of "int" is ambiguous:
       it would escape the scope of its equation
|}]

(* Ok *)
let f (type a b) (w1 : (a, b -> b) eq) (w2 : (a, int -> int) eq) (g : a) : b =
   let Refl = w2 in let Refl = w1 in g 3;;
[%%expect{|
val f : ('a, 'b -> 'b) eq -> ('a, int -> int) eq -> 'a -> 'b = <fun>
|}]


(* #14307 *)

type ('a, 'b) eq = Refl : ('a, 'a) eq

let choose3 (x : 'a) (y : 'a) (z : 'a) : 'a = x

let f
    (type a b c d)
    (w1 : (a, b) eq)
    (w2 : (c, d -> a) eq)
    (x1 : c)
    (x2 : d -> a)
    (x3 : d -> b)
  =
  match w2 with
  | Refl ->
    let r = ref None in
    (match w1 with
    | Refl ->
      let _ =
       fun z ->
        r := Some (choose3 x1 x2 x3 z);
        ()
      in
      ())
;;


[%%expect{|
type ('a, 'b) eq = Refl : ('a, 'a) eq
val choose3 : 'a -> 'a -> 'a -> 'a = <fun>
Line 20, characters 18-38:
20 |         r := Some (choose3 x1 x2 x3 z);
                       ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type "'a"
       This instance of "a" is ambiguous:
       it would escape the scope of its equation
|}]
