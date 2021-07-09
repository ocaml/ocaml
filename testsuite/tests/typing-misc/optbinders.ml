(* TEST
   * expect
*)

(* Optional binders can be used in value declarations,
   and signatures are equivalent with or without them. *)
module type Id1 = sig val id : 'a -> 'a end
module type Id2 = sig val id : 'a . 'a -> 'a end
module F (X : Id1) : Id2 = X
module G (X : Id2) : Id1 = X
module Id : Id2 = struct let id x = x end
[%%expect{|
module type Id1 = sig val id : 'a -> 'a end
module type Id2 = sig val id : 'a -> 'a end
module F : functor (X : Id1) -> Id2
module G : functor (X : Id2) -> Id1
module Id : Id2
|}]


(* If present, the variables must be universally quantified *)
type 'a constrained = string constraint 'a = int
module type Ok_constraint = sig val c : 'a constrained end
[%%expect{|
type 'a constrained = string constraint 'a = int
module type Ok_constraint = sig val c : int constrained end
|}]
module type Bad_constraint = sig val c : 'a . 'a constrained end
[%%expect{|
Line 1, characters 41-60:
1 | module type Bad_constraint = sig val c : 'a . 'a constrained end
                                             ^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a cannot be generalized: it is bound to
       int.
|}]

(* with the usual caveat for row variables *)
module type Row = sig val poly : 'a 'b . ([> `Foo of int] as 'a) * 'b end
module type NotRow = sig val poly : 'a 'b . (int as 'a) * 'b end
[%%expect{|
module type Row = sig val poly : [> `Foo of int ] * 'b end
Line 2, characters 36-60:
2 | module type NotRow = sig val poly : 'a 'b . (int as 'a) * 'b end
                                        ^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a cannot be generalized: it is bound to
       int.
|}]

(* If present, the quantifier must quantify all variables *)
module type F1 = sig
  val four : 'a 'b 'c 'd . 'a -> 'b -> 'c -> 'd -> 'a * 'b * 'c * 'd
end
[%%expect{|
module type F1 = sig val four : 'a -> 'b -> 'c -> 'd -> 'a * 'b * 'c * 'd end
|}]
;;
module type F2 = sig
  val four : 'a 'b 'd . 'a -> 'b -> 'c -> 'd -> 'a * 'b * 'c * 'd
end
[%%expect{|
Line 2, characters 36-38:
2 |   val four : 'a 'b 'd . 'a -> 'b -> 'c -> 'd -> 'a * 'b * 'c * 'd
                                        ^^
Error: The type variable 'c is unbound in this type declaration.
|}]


(* Explicit quantifiers may also be used in external definitions *)
module Ident : sig
  external identity : 'a . 'a -> 'a = "%identity"
end = struct
  external identity : 'a . 'a -> 'a = "%identity"
end
[%%expect{|
module Ident : sig external identity : 'a -> 'a = "%identity" end
|}]


(* Explicit quantifiers may also be used in GADTs *)
type g1 = Foo : 'a * ('a -> unit) -> g1
type g2 = g1 = Foo : 'a . 'a * ('a -> unit) -> g2
type g3 = g2 = Foo : 'b 'c 'd . 'd * ('d -> unit) -> g3
let intro = Foo (5, print_int)
let elim (Foo (x, f)) = f x
[%%expect{|
type g1 = Foo : 'a * ('a -> unit) -> g1
type g2 = g1 = Foo : 'a * ('a -> unit) -> g2
type g3 = g2 = Foo : 'd * ('d -> unit) -> g3
val intro : g3 = Foo (<poly>, <fun>)
val elim : g3 -> unit = <fun>
|}]

(* In GADT syntax, all type variables must be bound, even parameters *)
type 'a t =
  | Ok1 : 'b 'a . 'a -> 'a t
  | Ok2 of 'a
  | Bad : 'b . 'a -> 'a t
[%%expect{|
Line 4, characters 15-17:
4 |   | Bad : 'b . 'a -> 'a t
                   ^^
Error: The type variable 'a is unbound in this type declaration.
|}]
