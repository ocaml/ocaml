(* TEST
   * expect
*)

type 'a t =
  | A: [`a|`z] t
  | B: [`b|`z] t
;;
[%%expect{|
type 'a t = A : [ `a | `z ] t | B : [ `b | `z ] t
|}];;

let fn: type a. a t -> a -> int = fun x y ->
  match (x, y) with
  | (A, `a)
  | (B, `b) -> 0
  | (A, `z)
  | (B, `z) -> 1
;;
[%%expect{|
Uncaught exception: File "typing/patterns.ml", line 199, characters 21-27: Assertion failed

|}];;
