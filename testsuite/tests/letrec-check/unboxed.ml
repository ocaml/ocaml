(* TEST
   * expect
*)

type t = {x: int64} [@@unboxed]
let rec x = {x = y} and y = 3L;;
[%%expect{|
type t = { x : int64; } [@@unboxed]
Line 2, characters 12-19:
  let rec x = {x = y} and y = 3L;;
              ^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

type r = A of r [@@unboxed]
let rec y = A y;;
[%%expect{|
type r = A of r [@@unboxed]
Line 2, characters 12-15:
  let rec y = A y;;
              ^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

type a = {a: b }[@@unboxed]
and b = X of a | Y

let rec a =
  {a=
    (if Sys.opaque_identity true then
       X a
     else
       Y)};;
[%%expect{|
type a = { a : b; } [@@unboxed]
and b = X of a | Y
Line 5, characters 2-75:
  ..{a=
      (if Sys.opaque_identity true then
         X a
       else
         Y)}..
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

type d = D of e [@@unboxed]
and e = V of d | W;;

let rec d =
  D
    (if Sys.opaque_identity true then
       V d
     else
       W);;
[%%expect{|
type d = D of e [@@unboxed]
and e = V of d | W
Line 5, characters 2-72:
  ..D
      (if Sys.opaque_identity true then
         V d
       else
         W)..
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;
