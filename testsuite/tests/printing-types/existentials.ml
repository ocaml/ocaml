(* TEST
 expect;
*)

type foo1 =
  | Foo : ('a * 'b * 'c * 'd * 'e * 'f) -> foo1

let bar1 x =
  match x with
  | Foo a -> a + 1
  | _ -> 0
;;
[%%expect {|
type foo1 = Foo : ('a * 'b * 'c * 'd * 'e * 'f) -> foo1
Line 6, characters 13-14:
6 |   | Foo a -> a + 1
                 ^
Error: The value "a" has type "$a * $b * $c * $d * $e * $f"
       but an expression was expected of type "int"
       Hint: "$a", "$b", "$c", "$d", "$e" and "$f" are existential types
         bound by the constructor "Foo".
|}]

type foo2 =
  | Foo1 : 'a -> foo2
  | Foo2 : 'a -> foo2
  | Foo3 : 'a -> foo2
  | Foo4 : 'a -> foo2
  | Foo5 : 'a -> foo2
  | Foo6 : 'a -> foo2
  | Foo7 : 'a -> foo2

let bar2 x =
  match x with
  | Foo1 a1, Foo2 a2, Foo3 a3, Foo4 a4, Foo5 a5, Foo6 a6, Foo7 a7 ->
      let x = (a1, a2, a3, a4, a5, a6, a7) in x + 1
  | _ -> 0
;;
[%%expect {|
type foo2 =
    Foo1 : 'a -> foo2
  | Foo2 : 'a -> foo2
  | Foo3 : 'a -> foo2
  | Foo4 : 'a -> foo2
  | Foo5 : 'a -> foo2
  | Foo6 : 'a -> foo2
  | Foo7 : 'a -> foo2
Line 13, characters 46-47:
13 |       let x = (a1, a2, a3, a4, a5, a6, a7) in x + 1
                                                   ^
Error: The value "x" has type "$a * $a1 * $a2 * $a3 * $a4 * $a5 * $a6"
       but an expression was expected of type "int"
       Hint: "$a" is an existential type bound by the constructor "Foo1".
       Hint: "$a1" is an existential type bound by the constructor "Foo2".
       Hint: "$a2" is an existential type bound by the constructor "Foo3".
       Hint: "$a3" is an existential type bound by the constructor "Foo4".
       Hint: "$a4" is an existential type bound by the constructor "Foo5".
       Hint: "$a5" is an existential type bound by the constructor "Foo6".
       Hint: "$a6" is an existential type bound by the constructor "Foo7".
|}]

type foo3 =
  | Foo1 : ('a * 'b * 'c * 'd * 'e * 'f) -> foo3
  | Foo2 : ('a * 'b * 'c * 'd * 'e * 'f) -> foo3
  | Foo3 : ('a * 'b * 'c * 'd * 'e * 'f) -> foo3
  | Foo4 : ('a * 'b * 'c * 'd * 'e * 'f) -> foo3
  | Foo5 : ('a * 'b * 'c * 'd * 'e * 'f) -> foo3
  | Foo6 : ('a * 'b * 'c * 'd * 'e * 'f) -> foo3
  | Foo7 : ('a * 'b * 'c * 'd * 'e * 'f) -> foo3

let bar2 x =
  match x with
  | Foo1 a1, Foo2 a2, Foo3 a3, Foo4 a4, Foo5 a5, Foo6 a6, Foo7 a7 ->
      let x = (a1, a2, a3, a4, a5, a6, a7) in x + 1
  | _ -> 0
;;
[%%expect {|
type foo3 =
    Foo1 : ('a * 'b * 'c * 'd * 'e * 'f) -> foo3
  | Foo2 : ('a * 'b * 'c * 'd * 'e * 'f) -> foo3
  | Foo3 : ('a * 'b * 'c * 'd * 'e * 'f) -> foo3
  | Foo4 : ('a * 'b * 'c * 'd * 'e * 'f) -> foo3
  | Foo5 : ('a * 'b * 'c * 'd * 'e * 'f) -> foo3
  | Foo6 : ('a * 'b * 'c * 'd * 'e * 'f) -> foo3
  | Foo7 : ('a * 'b * 'c * 'd * 'e * 'f) -> foo3
Line 13, characters 46-47:
13 |       let x = (a1, a2, a3, a4, a5, a6, a7) in x + 1
                                                   ^
Error: The value "x" has type
         "($a * $b * $c * $d * $e * $f) *
         ($a1 * $b1 * $c1 * $d1 * $e1 * $f1) *
         ($a2 * $b2 * $c2 * $d2 * $e2 * $f2) *
         ($a3 * $b3 * $c3 * $d3 * $e3 * $f3) *
         ($a4 * $b4 * $c4 * $d4 * $e4 * $f4) *
         ($a5 * $b5 * $c5 * $d5 * $e5 * $f5) *
         ($a6 * $b6 * $c6 * $d6 * $e6 * $f6)"
       but an expression was expected of type "int"
       Hint: "$a", "$b", "$c", "$d", "$e" and "$f" are existential types
         bound by the constructor "Foo1".
       Hint: "$a1", "$b1", "$c1", "$d1", "$e1" and "$f1" are existential types
         bound by the constructor "Foo2".
       Hint: "$a2", "$b2", "$c2", "$d2", "$e2" and "$f2" are existential types
         bound by the constructor "Foo3".
       Hint: "$a3", "$b3", "$c3", "$d3", "$e3" and "$f3" are existential types
         bound by the constructor "Foo4".
       Hint: "$a4", "$b4", "$c4", "$d4", "$e4" and "$f4" are existential types
         bound by the constructor "Foo5".
       Hint: "$a5", "$b5", "$c5", "$d5", "$e5" and "$f5" are existential types
         bound by the constructor "Foo6".
       Hint: "$a6", "$b6", "$c6", "$d6", "$e6" and "$f6" are existential types
         bound by the constructor "Foo7".
|}]
