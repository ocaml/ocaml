(* TEST
   expect;
*)

(* Constructor with labeled arguments (disallowed) *)

type ('a, 'b) pair = Pair of 'a * 'b
let x = Pair (~x: 5, 2)

[%%expect{|
type ('a, 'b) pair = Pair of 'a * 'b
Line 2, characters 8-23:
2 | let x = Pair (~x: 5, 2)
            ^^^^^^^^^^^^^^^
Error: Constructors cannot have labeled arguments.
       Consider using an inline record instead.
|}]

(* Labeled tuple pattern in constructor pattern, with the same arity as the
   constructor. This is intentionally disallowed. *)
let f = function
| Pair (~x:5, 2) -> true
| _ -> false
[%%expect{|
Line 2, characters 2-16:
2 | | Pair (~x:5, 2) -> true
      ^^^^^^^^^^^^^^
Error: Constructors cannot have labeled arguments.
       Consider using an inline record instead.
|}]

(* Labeled tuple patterns in constructor patterns with that can unify with the
   constructor pattern type. *)
let f = function
| Some (~x:5, 2) -> true
| _ -> false
[%%expect{|
val f : (x:int * int) option -> bool = <fun>
|}]


type t = Foo of (x:int * int)
let f = function
| Foo (~x:5, 2) -> true
| _ -> false
[%%expect{|
type t = Foo of (x:int * int)
val f : t -> bool = <fun>
|}]

let _ = f (Foo (~x:5,2))
let _ = f (Foo (~x:4,2))
let _ = f (Foo (~x:5,1))
[%%expect{|
- : bool = true
- : bool = false
- : bool = false
|}]

let _ = f (Foo (5,1))
[%%expect{|
Line 1, characters 15-20:
1 | let _ = f (Foo (5,1))
                   ^^^^^
Error: This expression has type "'a * 'b"
       but an expression was expected of type "x:int * int"
|}]

let _ = f (Foo (5,~x:1))
[%%expect{|
Line 1, characters 15-23:
1 | let _ = f (Foo (5,~x:1))
                   ^^^^^^^^
Error: This expression has type "'a * x:'b"
       but an expression was expected of type "x:int * int"
|}]

let _ = f (Foo (5,~y:1))
[%%expect{|
Line 1, characters 15-23:
1 | let _ = f (Foo (5,~y:1))
                   ^^^^^^^^
Error: This expression has type "'a * y:'b"
       but an expression was expected of type "x:int * int"
|}]
