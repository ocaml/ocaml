(* TEST
 expect;
*)

type (_, _) eq = Eq : ('a, 'a) eq | Neq : int -> ('a, 'b) eq;;
type 'a t;;
let f (type a) (Neq n : (a, a t) eq) = n;;   (* warn! *)
[%%expect{|
type (_, _) eq = Eq : ('a, 'a) eq | Neq : int -> ('a, 'b) eq
type 'a t
Line 3, characters 15-36:
3 | let f (type a) (Neq n : (a, a t) eq) = n;;   (* warn! *)
                   ^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Eq"

val f : ('a, 'a t) eq -> int = <fun>
|}];;

module F (T : sig type _ t end) = struct
 let f (type a) (Neq n : (a, a T.t) eq) = n  (* warn! *)
end;;
[%%expect{|
Line 2, characters 16-39:
2 |  let f (type a) (Neq n : (a, a T.t) eq) = n  (* warn! *)
                    ^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Eq"

module F : (T : sig type _ t end) -> sig val f : ('a, 'a T.t) eq -> int end
|}];;
