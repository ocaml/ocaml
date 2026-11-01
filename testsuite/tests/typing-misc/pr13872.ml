(* TEST
 expect;
*)

type x=T
type a=x=T
type y=x
type b=y=T

[%%expect{|
type x = T
type a = x = T
type y = x
type b = y = T
|}]

type x' = private T

[%%expect{|
type x' = private T
|}]

(* Privacy is preserved *)

type a' = x' = T

[%%expect{|
Line 1, characters 0-16:
1 | type a' = x' = T
    ^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "x'"
       Private variant constructor(s) would be revealed.
|}]

(* Even through aliases*)

type y = x'
type b = y = T

[%%expect{|
type y = x'
Line 2, characters 0-14:
2 | type b = y = T
    ^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "y"
       Private variant constructor(s) would be revealed.
|}]

type x_p = private x
type b = x_p = private T

[%%expect{|
type x_p = private x
Line 2, characters 0-24:
2 | type b = x_p = private T
    ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "x_p"
       The type "x" is not equal to the type "x_p"
|}]

type x_p = private x
type b = x_p = T

[%%expect{|
type x_p = private x
Line 2, characters 0-16:
2 | type b = x_p = T
    ^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "x_p"
       The type "x" is not equal to the type "x_p"
|}]

type ('a, 'b) t = A of 'a | B of 'b
type ('a, 'b) t_rev = ('b, 'a) t
type ('a, 'b) q = ('a, 'b) t_rev = A of 'a | B of 'b

[%%expect{|
type ('a, 'b) t = A of 'a | B of 'b
type ('a, 'b) t_rev = ('b, 'a) t
Line 3, characters 0-52:
3 | type ('a, 'b) q = ('a, 'b) t_rev = A of 'a | B of 'b
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         "('a, 'b) t_rev"
       The representation of "t" cannot be used in the definition of this type, because
         "('a, 'b) q" is not an alias of "('b, 'a) t".  Their parameters differ:
         The type "'a" is not equal to the type "'b"
       When re-exporting a type representation, each type equation leading to
       the original representation must be an alias defining a type
       with the same parameters, in the same order, with the same constraints.
|}]

type ('a, 'b) t = A of 'a | B of 'b
type 'a t_eq = ('a, 'a) t
type ('a, 'b) q = 'a t_eq = A of 'a | B of 'b

[%%expect{|
type ('a, 'b) t = A of 'a | B of 'b
type 'a t_eq = ('a, 'a) t
Line 3, characters 0-45:
3 | type ('a, 'b) q = 'a t_eq = A of 'a | B of 'b
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "'a t_eq"
       They have different arities.
|}]

type ('a, 'b) t = A of 'a | B of 'b
type 'a t_eq = ('a, 'a) t
type ('a, 'b) q' = 'a t_eq
type ('a, 'b) q = ('a, 'b) q' = A of 'a | B of 'b

[%%expect{|
type ('a, 'b) t = A of 'a | B of 'b
type 'a t_eq = ('a, 'a) t
type ('a, 'b) q' = 'a t_eq
Line 4, characters 0-49:
4 | type ('a, 'b) q = ('a, 'b) q' = A of 'a | B of 'b
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         "('a, 'b) q'"
       The representation of "t_eq" cannot be used in the definition of this type, because
         "('a, 'b) q" is not an alias of "'a t_eq".  They have different arities.
       When re-exporting a type representation, each type equation leading to
       the original representation must be an alias defining a type
       with the same parameters, in the same order, with the same constraints.
|}]

(* Reverse the reverse, so this could work if we can be more sophisticated
   about expanding manifest types. *)

type ('a0, 'b0) t = A of 'a0 | B of 'b0
type ('a1, 'b1) t_rev = ('b1, 'a1) t
type ('a2, 'b2) q = ('b2, 'a2) t_rev = A of 'a2 | B of 'b2

[%%expect{|
type ('a0, 'b0) t = A of 'a0 | B of 'b0
type ('a1, 'b1) t_rev = ('b1, 'a1) t
Line 3, characters 0-58:
3 | type ('a2, 'b2) q = ('b2, 'a2) t_rev = A of 'a2 | B of 'b2
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         "('b2, 'a2) t_rev"
       Their parameters differ:
       The type "'b2" is not equal to the type "'a2"
|}]

(* Reverse the reverse using a separate abbreviation *)

type ('a0, 'b0) t = A of 'a0 | B of 'b0
type ('a1, 'b1) t_rev = ('b1, 'a1) t
type ('a2, 'b2) t_rev_rev = ('b2, 'a2) t_rev
type ('a3, 'b3) q = ('a3, 'b3) t_rev_rev = A of 'a3 | B of 'b3

[%%expect{|
type ('a0, 'b0) t = A of 'a0 | B of 'b0
type ('a1, 'b1) t_rev = ('b1, 'a1) t
type ('a2, 'b2) t_rev_rev = ('b2, 'a2) t_rev
Line 4, characters 0-62:
4 | type ('a3, 'b3) q = ('a3, 'b3) t_rev_rev = A of 'a3 | B of 'b3
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         "('a3, 'b3) t_rev_rev"
       The representation of "t_rev" cannot be used in the definition of this type, because
         "('a2, 'b2) q" is not an alias of "('b2, 'a2) t_rev".
         Their parameters differ: The type "'a2" is not equal to the type "'b2"
       When re-exporting a type representation, each type equation leading to
       the original representation must be an alias defining a type
       with the same parameters, in the same order, with the same constraints.
|}]

(* Add one more indirection *)

type ('a, 'b) t = A of 'a | B of 'b
type ('a, 'b) t_rev = ('b, 'a) t
type ('a, 'b) t_rev_rev = ('b, 'a) t_rev
type ('a, 'b) q' = ('a, 'b) t_rev_rev
type ('a, 'b) q = ('a, 'b) q' = A of 'a | B of 'b

[%%expect{|
type ('a, 'b) t = A of 'a | B of 'b
type ('a, 'b) t_rev = ('b, 'a) t
type ('a, 'b) t_rev_rev = ('b, 'a) t_rev
type ('a, 'b) q' = ('a, 'b) t_rev_rev
Line 5, characters 0-49:
5 | type ('a, 'b) q = ('a, 'b) q' = A of 'a | B of 'b
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         "('a, 'b) q'"
       The representation of "t_rev" cannot be used in the definition of this type, because
         "('a, 'b) t_rev_rev" is not an alias of "('b, 'a) t_rev".
         Their parameters differ: The type "'a" is not equal to the type "'b"
       When re-exporting a type representation, each type equation leading to
       the original representation must be an alias defining a type
       with the same parameters, in the same order, with the same constraints.
|}]

type ('a, 'b) t = A of 'a | B of 'b
type ('a, 'b) t_same = ('a, 'b) t
type ('a, 'b) q = ('a, 'b) t_same = A of 'a | B of 'b

[%%expect{|
type ('a, 'b) t = A of 'a | B of 'b
type ('a, 'b) t_same = ('a, 'b) t
type ('a, 'b) q = ('a, 'b) t_same = A of 'a | B of 'b
|}]

(* Extra constraints are rejected *)

type ('a, 'b) t = A of 'a | B of 'b
type ('a, 'b) t_same = ('a, 'b) t constraint 'a = int
type ('a, 'b) q = ('a, 'b) t_same = A of 'a | B of 'b

[%%expect{|
type ('a, 'b) t = A of 'a | B of 'b
type ('a, 'b) t_same = ('a, 'b) t constraint 'a = int
Line 3, characters 0-53:
3 | type ('a, 'b) q = ('a, 'b) t_same = A of 'a | B of 'b
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         "(int, 'b) t_same"
       The representation of "t" cannot be used in the definition of this type, because
         "(int, 'b) q" is not an alias of "(int, 'b) t".
         "(int, 'b) t" is not an unmodified instantiation of "('a, 'b) t"
         Their parameters differ: The type "'a" is not equal to the type "int"
       When re-exporting a type representation, each type equation leading to
       the original representation must be an alias defining a type
       with the same parameters, in the same order, with the same constraints.
|}]

(* They are even rejected if they don't change the representation *)

type ('a, 'b, 'c) t = A of 'a | B of 'b
type ('a, 'b, 'c) t_same = ('a, 'b, 'c) t constraint 'c = int
type ('a, 'b, 'c) q = ('a, 'b, 'c) t_same = A of 'a | B of 'b

[%%expect{|
type ('a, 'b, 'c) t = A of 'a | B of 'b
type ('a, 'b, 'c) t_same = ('a, 'b, 'c) t constraint 'c = int
Line 3, characters 0-61:
3 | type ('a, 'b, 'c) q = ('a, 'b, 'c) t_same = A of 'a | B of 'b
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         "('a, 'b, int) t_same"
       The representation of "t" cannot be used in the definition of this type, because
         "('a, 'b, int) q" is not an alias of "('a, 'b, int) t".
         "('a, 'b, int) t" is not an unmodified instantiation of "('a, 'b, 'c) t"
         Their parameters differ: The type "'c" is not equal to the type "int"
       When re-exporting a type representation, each type equation leading to
       the original representation must be an alias defining a type
       with the same parameters, in the same order, with the same constraints.
|}]

(* Of course, this works for records *)

type ('a, 'b, 'c) t = { a : 'a; b : 'b }
type ('a, 'b, 'c) t_same = ('a, 'b, 'c) t constraint 'c = int
type ('a, 'b, 'c) q = ('a, 'b, 'c) t_same = { a : 'a; b : 'b }

[%%expect{|
type ('a, 'b, 'c) t = { a : 'a; b : 'b; }
type ('a, 'b, 'c) t_same = ('a, 'b, 'c) t constraint 'c = int
Line 3, characters 0-62:
3 | type ('a, 'b, 'c) q = ('a, 'b, 'c) t_same = { a : 'a; b : 'b }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         "('a, 'b, int) t_same"
       The representation of "t" cannot be used in the definition of this type, because
         "('a, 'b, int) q" is not an alias of "('a, 'b, int) t".
         "('a, 'b, int) t" is not an unmodified instantiation of "('a, 'b, 'c) t"
         Their parameters differ: The type "'c" is not equal to the type "int"
       When re-exporting a type representation, each type equation leading to
       the original representation must be an alias defining a type
       with the same parameters, in the same order, with the same constraints.
|}]
