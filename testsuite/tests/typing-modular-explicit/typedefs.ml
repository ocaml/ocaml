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

type 'a t5 = (module M : T with type t = 'a) -> 'a -> 'a

type 'a t6 = 'a -> (module M : T with type t = 'a) -> 'a

type t7 = < m : 'a. (module M : T with type t = 'a) -> 'a >

type t8 =
    A of ((module T : T) -> (module A : Add with type t = T.t) -> A.t -> A.t)
  | B of t1

type t9 = C of 'a constraint 'a = (module T : T) -> T.t -> T.t

(* Here we test having the same type but with a slightly different definition *)
type t10 = t8 =
    A of ((module T : T) -> (module Add with type t = T.t) -> T.t -> T.t)
  | B of ((module T : T) -> (module Add with type t = T.t) -> T.t -> T.t)

[%%expect{|
type t0 = (module M : T) -> M.t -> M.t
type t1 = (module T : T) -> (module A : Add with type t = T.t) -> A.t -> A.t
type _ t2 = A : ((module M : T) -> M.t) t2
type t3 = [ `A of (module M : T) -> (module N : T with type t = M.t) -> N.t ]
type t4 = < m : (module M : T) -> M.t >
type 'a t5 = (module M : T with type t = 'a) -> 'a -> 'a
type 'a t6 = 'a -> (module M : T with type t = 'a) -> 'a
type t7 = < m : 'a. (module M : T with type t = 'a) -> 'a >
type t8 =
    A of ((module T : T) -> (module A : Add with type t = T.t) -> A.t -> A.t)
  | B of t1
type t9 = C of ((module T : T) -> T.t -> T.t)
type t10 =
  t8 =
    A of ((module T : T) -> (module Add with type t = T.t) -> T.t -> T.t)
  | B of ((module T : T) -> (module Add with type t = T.t) -> T.t -> T.t)
|}]

(** Test constraint check, one success and the next one is a fail  *)
let t9_success (x : (module T : T) -> T.t -> T.t) = C x

[%%expect{|
val t9_success : ((module T : T) -> T.t -> T.t) -> t9 = <fun>
|}]

let t9_fail (x : (module T : T) -> T.t -> int) = C x

[%%expect{|
Line 1, characters 51-52:
1 | let t9_fail (x : (module T : T) -> T.t -> int) = C x
                                                       ^
Error: This expression has type "(module T : T) -> T.t -> int"
       but an expression was expected of type "(module T : T) -> T.t -> T.t"
       Type "int" is not compatible with type "T.t"
|}]

(** Test about invalid types definitions *)

type t_fail1 = (module M : T) -> M.a

[%%expect{|
Line 3, characters 33-36:
3 | type t_fail1 = (module M : T) -> M.a
                                     ^^^
Error: Unbound type constructor "M.a"
|}]

(* N is not defined before *)
type t_fail2 = (module M : T) -> N.t

[%%expect{|
Line 1, characters 33-36:
1 | type t_fail2 = (module M : T) -> N.t
                                     ^^^
Error: Unbound module "N"
|}]

(* M is not defined before *)
type t_fail3 = < m : (module M : T) -> M.t; n : M.t >

[%%expect{|
Line 1, characters 48-51:
1 | type t_fail3 = < m : (module M : T) -> M.t; n : M.t >
                                                    ^^^
Error: Unbound module "M"
|}]

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

(* tests about variance *)

module type V = sig
  type +'a p
  type -'a n
  type !'a i
end

(* this test is here to compare that both behave the same way *)
module type F = functor (X : V) -> sig
  type +'a t_pos = unit -> 'a X.p
  type -'a t_neg = unit -> 'a X.n
  type !'a t_inj = unit -> 'a X.i
  type -'a t_npos = unit -> 'a X.p -> unit
  type +'a t_pneg = unit -> 'a X.n -> unit
end

type +'a t_pos = (module X : V) -> 'a X.p
type -'a t_neg = (module X : V) -> 'a X.n
type !'a t_inj = (module X : V) -> 'a X.i
type -'a t_npos = (module X : V) -> 'a X.p -> unit
type +'a t_pneg = (module X : V) -> 'a X.n -> unit

[%%expect{|
module type V = sig type +'a p type -'a n type !'a i end
module type F =
  functor (X : V) ->
    sig
      type 'a t_pos = unit -> 'a X.p
      type 'a t_neg = unit -> 'a X.n
      type 'a t_inj = unit -> 'a X.i
      type 'a t_npos = unit -> 'a X.p -> unit
      type 'a t_pneg = unit -> 'a X.n -> unit
    end
type 'a t_pos = (module X : V) -> 'a X.p
type 'a t_neg = (module X : V) -> 'a X.n
type 'a t_inj = (module X : V) -> 'a X.i
type 'a t_npos = (module X : V) -> 'a X.p -> unit
type 'a t_pneg = (module X : V) -> 'a X.n -> unit
|}]

type +'a t_pos = (module X : V) -> 'a X.p -> unit

[%%expect{|
Line 1, characters 0-49:
1 | type +'a t_pos = (module X : V) -> 'a X.p -> unit
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is contravariant.
|}]

type -'a t_neg = (module X : V) -> 'a X.n -> unit

[%%expect{|
Line 1, characters 0-49:
1 | type -'a t_neg = (module X : V) -> 'a X.n -> unit
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be contravariant,
       but it is covariant.
|}]


(** Here we test compatibility between different type definitions *)

let id_fail1 (x : t0) : _ t5 = x

[%%expect{|
Line 3, characters 31-32:
3 | let id_fail1 (x : t0) : _ t5 = x
                                   ^
Error: This expression has type "t0" = "(module M : T) -> M.t -> M.t"
       but an expression was expected of type
         "'a t5" = "(module M : T with type t = 'a) -> 'a -> 'a"
       Type "(module T)" is not compatible with type
         "(module T with type t = 'a)"
|}]

let id_fail2 (x : _ t5) : t0 = x

[%%expect{|
Line 1, characters 31-32:
1 | let id_fail2 (x : _ t5) : t0 = x
                                   ^
Error: This expression has type
         "'a t5" = "(module M : T with type t = 'a) -> 'a -> 'a"
       but an expression was expected of type
         "t0" = "(module M : T) -> M.t -> M.t"
       Type "(module T with type t = 'a)" is not compatible with type
         "(module T)"
|}]


(* This test check that no scope escape happens when trying to replace 'a by
    A.t *)
type 'a wrapper = 'a

type should_succeed2 =
  (module A:T) -> A.t wrapper

[%%expect{|
type 'a wrapper = 'a
type should_succeed2 = (module A : T) -> A.t wrapper
|}]
