(* TEST
   * expect
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
