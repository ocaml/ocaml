(* TEST
 expect;
*)

type ('a,'at,'any,'en) t = A of 'an
[%%expect {|
Line 1, characters 32-35:
1 | type ('a,'at,'any,'en) t = A of 'an
                                    ^^^
Error: The type variable 'an is unbound in this type declaration.
Hint: Did you mean 'a, 'any, 'at or 'en?
|}
]

type mismatched = [< `A of int | `B of float > `B `C]
[%%expect {|
Line 1, characters 18-53:
1 | type mismatched = [< `A of int | `B of float > `B `C]
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The constructor C is missing from the upper bound (between '<'
       and '>') of this polymorphic variant but is present in
       its lower bound (after '>').
       Hint: Either add `C in the upper bound, or remove it
       from the lower bound.
|}]

type ('_a) underscored = A of '_a
[%%expect {|
Line 1, characters 6-9:
1 | type ('_a) underscored = A of '_a
          ^^^
Error: The type variable name '_a is not allowed in programs
|}]

(* The next two hit the unification error case at the end of
   Typetexp.globalize_used_variables. *)
let f (x: int as 'a) (y: float as 'a) = (x,y)
[%%expect{|
Line 1, characters 25-36:
1 | let f (x: int as 'a) (y: float as 'a) = (x,y)
                             ^^^^^^^^^^^
Error: This type float should be an instance of type int
|}]

type 'a t1 = 'a constraint 'a = 'b list
type 'a t2 = 'a constraint 'a = 'b option

let f (x : 'a t1) = (assert false : 'a t2)
[%%expect{|
type 'a t1 = 'a constraint 'a = 'b list
type 'a t2 = 'a constraint 'a = 'b option
Line 4, characters 36-38:
4 | let f (x : 'a t1) = (assert false : 'a t2)
                                        ^^
Error: This type 'a option should be an instance of type 'b list
|}]
