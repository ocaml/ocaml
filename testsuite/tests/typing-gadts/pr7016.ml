(* TEST
 expect;
*)

type (_, _) t =
  | Nil : ('tl, 'tl) t
  | Cons : 'a * ('b, 'tl) t -> ('a * 'b, 'tl) t;;

let get1 (Cons (x, _) : (_ * 'a, 'a) t) = x ;; (* warn, cf PR#6993 *)
[%%expect{|
type (_, _) t =
    Nil : ('tl, 'tl) t
  | Cons : 'a * ('b, 'tl) t -> ('a * 'b, 'tl) t
Line 5, characters 9-39:
5 | let get1 (Cons (x, _) : (_ * 'a, 'a) t) = x ;; (* warn, cf PR#6993 *)
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Nil

val get1 : ('a * 'a0, 'a0) t -> 'a = <fun>
|}];;

let get1' = function
  | (Cons (x, _) : (_ * 'a, 'a) t) -> x
  | Nil -> assert false ;; (* ok *)
[%%expect{|
Line 3, characters 4-7:
3 |   | Nil -> assert false ;; (* ok *)
        ^^^
Error: This pattern matches values of type "('a * 'a0, 'a * 'a0) t"
       but a pattern was expected which matches values of type
         "('a * 'a0, 'a0) t"
       The type variable "'a0" occurs inside "'a * 'a0"
|}];;
