(* TEST
 expect;
*)

type (_, _) eq = Refl : ('a, 'a) eq

(* Polymorphic univars are treated as locally abstract types*)

let sym : 'a 'b. ('a, 'b) eq -> ('b, 'a) eq =
  fun Refl -> Refl

let trans : 'a 'b 'c. ('a, 'b) eq -> ('b, 'c) eq -> ('a, 'c) eq =
  fun Refl Refl -> Refl
[%%expect {|
type (_, _) eq = Refl : ('a, 'a) eq
val sym : ('a, 'b) eq -> ('b, 'a) eq = <fun>
val trans : ('a, 'b) eq -> ('b, 'c) eq -> ('a, 'c) eq = <fun>
|}]


(* Scoped variables cannot clash with type constructor names *)
type t = A;;

let bad : 't. 't -> t =
  fun _ -> A
[%%expect {|
type t = A
Line 3, characters 20-21:
3 | let bad : 't. 't -> t =
                        ^
Error: In this scoped type, the constructor "t" is reserved for the polymorphic type variable "'t".
|}]
