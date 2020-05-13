(* TEST
   flags = " -w A -strict-sequence "
   * expect
*)

(* Exhaustiveness check is very slow *)
type _ t =
  A : int t | B : bool t | C : char t | D : float t
type (_,_,_,_) u = U : (int, int, int, int) u
type v = E | F | G
;;

let f : type a b c d e f g.
      a t * b t * c t * d t * e t * f t * g t * v
       * (a,b,c,d) u * (e,f,g,g) u -> int =
 function A, A, A, A, A, A, A, _, U, U -> 1
   | _, _, _, _, _, _, _, G, _, _ -> 1
   (*| _ -> _ *)
;;
[%%expect {|
type _ t = A : int t | B : bool t | C : char t | D : float t
type (_, _, _, _) u = U : (int, int, int, int) u
type v = E | F | G
Lines 10-11, characters 1-38:
10 | .function A, A, A, A, A, A, A, _, U, U -> 1
11 |    | _, _, _, _, _, _, _, G, _, _ -> 1
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(A, A, A, A, A, A, B, (E|F), _, _)
Line 11, characters 5-33:
11 |    | _, _, _, _, _, _, _, G, _, _ -> 1
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 56: this match case is unreachable.
Consider replacing it with a refutation case '<pat> -> .'
val f :
  'a t * 'b t * 'c t * 'd t * 'e t * 'f t * 'g t * v * ('a, 'b, 'c, 'd) u *
  ('e, 'f, 'g, 'g) u -> int = <fun>
|}]
