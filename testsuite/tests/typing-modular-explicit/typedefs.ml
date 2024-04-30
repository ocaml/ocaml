(* TEST
  expect;
*)

module type T = sig
  type t
end

module type Add = sig
  type t
  val add : t -> t -> t
end

[%%expect{|
module type T = sig type t end
module type Add = sig type t val add : t -> t -> t end
|}]

type t0 = (module M : T) -> M.t -> M.t

type t1 = (module T : T) -> (module A : Add with type t = T.t) -> A.t -> A.t

type _ t2 = A : ((module M : T) -> M.t) t2

type t3 = [`A of ((module M : T) -> (module N : T with type t = M.t) -> N.t)]

type t4 = < m : (module M : T) -> M.t >

type 'a t5 = (module M : T with type t = 'a) -> 'a

type 'a t6 = 'a -> (module M : T with type t = 'a) -> 'a

type t7 = < m : 'a. (module M : T with type t = 'a) -> 'a >

type t8 =
    A of ((module T : T) -> (module A : Add with type t = T.t) -> A.t -> A.t)
  | B of t1

type t9 = C of 'a constraint 'a = (module T : T) -> T.t -> T.t

type t10 = t8 =
    A of ((module T : T) -> (module A : Add with type t = T.t) -> A.t -> A.t)
  | B of ((module T : T) -> (module A : Add with type t = T.t) -> A.t -> A.t)

[%%expect{|
type t0 = (module M : T) -> M.t -> M.t
type t1 = (module T : T) -> (module A : Add with type t = T.t) -> A.t -> A.t
type _ t2 = A : ((module M : T) -> M.t) t2
type t3 = [ `A of (module M : T) -> (module N : T with type t = M.t) -> N.t ]
type t4 = < m : (module M : T) -> M.t >
type 'a t5 = (module M : T with type t = 'a) -> 'a
type 'a t6 = 'a -> (module M : T with type t = 'a) -> 'a
type t7 = < m : 'a. (module M : T with type t = 'a) -> 'a >
type t8 =
    A of ((module T : T) -> (module A : Add with type t = T.t) -> A.t -> A.t)
  | B of t1
type t9 = C of ((module T : T) -> T.t -> T.t)
type t10 =
  t8 =
    A of ((module T : T) -> (module A : Add with type t = T.t) -> A.t -> A.t)
  | B of ((module T : T) -> (module A : Add with type t = T.t) -> A.t -> A.t)
|}]

(* Test about invalid types *)

type t_fail1 = (module M : T) -> M.a

[%%expect{|
Line 1, characters 33-36:
1 | type t_fail1 = (module M : T) -> M.a
                                     ^^^
Error: Unbound type constructor "M.a"
|}]

type t_fail2 = (module M : T) -> N.t

[%%expect{|
Line 1, characters 33-36:
1 | type t_fail2 = (module M : T) -> N.t
                                     ^^^
Error: Unbound module "N"
|}]

type t_fail3 = < m : (module M : T) -> M.t; n : M.t >

[%%expect{|
Line 1, characters 48-51:
1 | type t_fail3 = < m : (module M : T) -> M.t; n : M.t >
                                                    ^^^
Error: Unbound module "M"
|}]

(* should this fail ? *)
type +'a t_fail4 = 'a -> (module M : T with type t = 'a) -> unit

[%%expect{|
Line 1, characters 0-64:
1 | type +'a t_fail4 = 'a -> (module M : T with type t = 'a) -> unit
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is injective invariant.
|}]

(* this one should fail but the error message might not be the best *)
type +'a t_fail5 = (module M : T with type t = 'a) -> M.t

[%%expect{|
Line 1, characters 0-57:
1 | type +'a t_fail5 = (module M : T with type t = 'a) -> M.t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is injective invariant.
|}]

type t_fail6 = (module M : T) -> 'a constraint 'a = M.t

[%%expect{|
Line 1, characters 52-55:
1 | type t_fail6 = (module M : T) -> 'a constraint 'a = M.t
                                                        ^^^
Error: Unbound module "M"
|}]

let id_fail1 (x : t1) : _ t5 = x

[%%expect{|
Line 1, characters 31-32:
1 | let id_fail1 (x : t1) : _ t5 = x
                                   ^
Error: This expression has type
         "t1" =
           "(module T : T) -> (module A : Add with type t = T.t) -> A.t -> A.t"
       but an expression was expected of type
         "'a t5" = "(module M : T with type t = 'a) -> 'a"
|}]

let id_fail2 (x : _ t5) : t1 = x

[%%expect{|
Line 1, characters 31-32:
1 | let id_fail2 (x : _ t5) : t1 = x
                                   ^
Error: This expression has type "'a t5" = "(module M : T with type t = 'a) -> 'a"
       but an expression was expected of type
         "t1" =
           "(module T : T) -> (module A : Add with type t = T.t) -> A.t -> A.t"
|}]
