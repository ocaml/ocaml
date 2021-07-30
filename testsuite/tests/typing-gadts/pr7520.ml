(* TEST
   * expect
*)

type ('a, 'b) eq = Refl : ('a, 'a) eq
type empty = (int, string) eq

let f = function `Foo (_ : empty) -> .
[%%expect{|
type ('a, 'b) eq = Refl : ('a, 'a) eq
type empty = (int, string) eq
val f : [< `Foo of empty ] -> 'a = <fun>
|}]
