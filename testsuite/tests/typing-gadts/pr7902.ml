(* TEST
   * expect
*)

type ('a, 'b) segment =
  | SegNil : ('a, 'a) segment
  | SegCons : ('a * 'a, 'b) segment -> ('a, 'b) segment

let color : type a b . (a, b) segment -> int = function
  | SegNil -> 0
  | SegCons SegNil -> 0
  | SegCons _ -> 0
[%%expect{|
type ('a, 'b) segment =
    SegNil : ('a, 'a) segment
  | SegCons : ('a * 'a, 'b) segment -> ('a, 'b) segment
val color : ('a, 'b) segment -> int = <fun>
|}]

(* Fail *)
let color (* : type a b . (a, b) segment -> int *) = function
  | SegNil -> 0
  | SegCons SegNil -> 0
  | SegCons _ -> 0
[%%expect{|
Line 3, characters 12-18:
3 |   | SegCons SegNil -> 0
                ^^^^^^
Error: This pattern matches values of type ('a * 'a, 'a * 'a) segment
       but a pattern was expected which matches values of type
         ('a * 'a, 'a) segment
       The type variable 'a occurs inside 'a * 'a
|}]
