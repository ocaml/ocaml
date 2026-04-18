(* TEST
 expect;
*)

(* alias to a variant *)
type t = T
module M : sig type u = T end = struct type u = t end

[%%expect{|
type t = T
module M : sig type u = T end
|}]

(* Check all permutations of privacy *)

type t = T
module M : sig type u = T end = struct type u = private t end

[%%expect{|
type t = T
Line 2, characters 32-61:
2 | module M : sig type u = T end = struct type u = private t end
                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type u = private t end
       is not included in
         sig type u = T end
       Type declarations do not match:
         type u = private t
       is not included in
         type u = T
       Private variant constructor(s) would be revealed.
|}]

type t = private T
module M : sig type u = T end = struct type u = t end

[%%expect{|
type t = private T
Line 2, characters 32-53:
2 | module M : sig type u = T end = struct type u = t end
                                    ^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type u = t end
       is not included in
         sig type u = T end
       Type declarations do not match:
         type u = t
       is not included in
         type u = T
       Private variant constructor(s) would be revealed.
|}]

type t = private T
module M : sig type u = T end = struct type u = private t end

[%%expect{|
type t = private T
Line 2, characters 32-61:
2 | module M : sig type u = T end = struct type u = private t end
                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type u = private t end
       is not included in
         sig type u = T end
       Type declarations do not match:
         type u = private t
       is not included in
         type u = T
       Private variant constructor(s) would be revealed.
|}]

type t = T
module M : sig type u = T end = struct type t' = private t type u = t' end

[%%expect{|
type t = T
Line 2, characters 32-74:
2 | module M : sig type u = T end = struct type t' = private t type u = t' end
                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t' = private t type u = t' end
       is not included in
         sig type u = T end
       Type declarations do not match:
         type u = t'
       is not included in
         type u = T
       Private variant constructor(s) would be revealed.
|}]


(* more complicated example from garrigue *)

module type S = sig type 'a t end
module Id (S : S) = S
module type Sp = sig type 'a t = A of 'a list end
module L = struct type 'a t = A of 'a list end
module M : Sp = Id(L)

[%%expect{|
module type S = sig type 'a t end
module Id : (S : S) -> sig type 'a t = 'a S.t end
module type Sp = sig type 'a t = A of 'a list end
module L : sig type 'a t = A of 'a list end
module M : Sp
|}]


module M : sig
  type ('a, 'b) t = A of 'a | B of 'b
  type ('a, 'b) t_rev = ('b, 'a) t
  type ('a, 'b) q = A of 'a | B of 'b
end = struct
  type ('a, 'b) t = A of 'a | B of 'b
  type ('a, 'b) t_rev = ('b, 'a) t
  type ('a, 'b) q = ('a, 'b) t_rev
end

[%%expect{|
Lines 5-9, characters 6-3:
5 | ......struct
6 |   type ('a, 'b) t = A of 'a | B of 'b
7 |   type ('a, 'b) t_rev = ('b, 'a) t
8 |   type ('a, 'b) q = ('a, 'b) t_rev
9 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type ('a, 'b) t = A of 'a | B of 'b
           type ('a, 'b) t_rev = ('b, 'a) t
           type ('a, 'b) q = ('a, 'b) t_rev
         end
       is not included in
         sig
           type ('a, 'b) t = A of 'a | B of 'b
           type ('a, 'b) t_rev = ('b, 'a) t
           type ('a, 'b) q = A of 'a | B of 'b
         end
       Type declarations do not match:
         type ('a, 'b) q = ('a, 'b) t_rev
       is not included in
         type ('a, 'b) q = A of 'a | B of 'b
       The representation of "t" cannot be used in the declaration of the second type, because
         "('a, 'b) t_rev" is not an alias of "('b, 'a) t".
         Their parameters differ: The type "'a" is not equal to the type "'b"
       When re-exporting a type representation, each type equation leading to
       the original representation must be an alias defining a type
       with the same parameters, in the same order, with the same constraints.
|}]

module X : sig
  type t = external "x"
end = struct
  type x = external "x"
  type t' = private x
  type t = t'
end ;;

[%%expect{|
Lines 3-7, characters 6-3:
3 | ......struct
4 |   type x = external "x"
5 |   type t' = private x
6 |   type t = t'
7 | end...
Error: Signature mismatch:
       Modules do not match:
         sig type x = external "x" type t' = private x type t = t' end
       is not included in
         sig type t = external "x" end
       Type declarations do not match:
         type t = t'
       is not included in
         type t = external "x"
       A private external type would be revealed.
|}]
