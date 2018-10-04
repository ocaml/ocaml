(* TEST
   * expect
*)

type ('a,'at,'any,'en) t = A of 'an
[%%expect {|
Line 1, characters 32-35:
  type ('a,'at,'any,'en) t = A of 'an
                                  ^^^
Error: The type variable 'an is unbound in this type declaration.
Hint: Did you mean 'a, 'any, 'at or 'en?
|}
]

type mismatched = [< `A of int | `B of float > `B `C]
[%%expect {|
Line 1, characters 18-53:
  type mismatched = [< `A of int | `B of float > `B `C]
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The constructor `C is missing from the upper bound of this
       polymorphic variant type. Upper and lower bounds are mismatched.
|}]
