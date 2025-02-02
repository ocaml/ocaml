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
val f : ('a, 'b -> 'b) eq -> ('a, int -> int) eq -> 'a -> 'b = <fun>
|}]
let f (type a b) (w1 : (a, b -> b) eq) (w2 : (a, int -> int) eq) (g : a) =
   let Refl = w2 in let Refl = w1 in g 3;;
[%%expect{|
val f : ('a, 'b -> 'b) eq -> ('a, int -> int) eq -> 'a -> int = <fun>
|}]

(* Ok *)
let f (type a b) (w1 : (a, b -> b) eq) (w2 : (a, int -> int) eq) (g : a) : b =
   let Refl = w2 in let Refl = w1 in g 3;;
[%%expect{|
val f : ('a, 'b -> 'b) eq -> ('a, int -> int) eq -> 'a -> 'b = <fun>
|}]

(* This should work. *)
let f (type a b) (w : (a, int -> int) eq) (g : a) =
   let Refl = w in g 3;;
[%%expect{|
val f : ('a, int -> int) eq -> 'a -> int = <fun>
|}]
