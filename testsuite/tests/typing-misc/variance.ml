(* TEST
   * expect
*)

(* #8698 *)

(* Actually, this is not a bug *)
type +'a t = [> `Foo of 'a -> unit] as 'a;;
[%%expect{|
type 'a t = 'a constraint 'a = [> `Foo of 'a -> unit ]
|}, Principal{|
type +'a t = 'a constraint 'a = [> `Foo of 'a -> unit ]
|}]

(* strengthening *)

type 'a t = (('a -> unit) -> unit);;
let tl = !(ref ([] : 'a t list));;
[%%expect{|
type 'a t = ('a -> unit) -> unit
val tl : '_a t list = []
|}]

type 'a u = U of (('a -> unit) -> unit);;
let ul = !(ref ([] : 'a u list));;
[%%expect{|
type 'a u = U of (('a -> unit) -> unit)
val ul : 'a u list = []
|}]

(* #11869 *)

module type s = sig type t end;;
type !'a t = (module s with type t = 'a);;
[%%expect{|
module type s = sig type t end
type 'a t = (module s with type t = 'a)
|}]

(* Composition *)
type -'a n
type +'a p
type !'a i

type +'a error_np = 'a n p;;
[%%expect{|
type -'a n
type +'a p
type !'a i
Line 5, characters 0-26:
5 | type +'a error_np = 'a n p;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is contravariant.
|}]


type +'a error_pn = 'a p n;;
[%%expect{|
Line 1, characters 0-26:
1 | type +'a error_pn = 'a p n;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is contravariant.
|}]

type -'a error_pp = 'a p p;;
[%%expect{|
Line 1, characters 0-26:
1 | type -'a error_pp = 'a p p;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be contravariant,
       but it is covariant.
|}]

type -'a error_nn = 'a n n;;
[%%expect{|
Line 1, characters 0-26:
1 | type -'a error_nn = 'a n n;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be contravariant,
       but it is covariant.
|}]

type !'a inj_in = 'a i n
[%%expect{|
Line 1, characters 0-24:
1 | type !'a inj_in = 'a i n
    ^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be injective invariant,
       but it is invariant.
|}]

type !'a inj_in = 'a n i
[%%expect{|
Line 1, characters 0-24:
1 | type !'a inj_in = 'a n i
    ^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be injective invariant,
       but it is invariant.
|}]

module Make_covariant(M: sig type 'a t end): sig
  type 'a i = 'a
  type +'a t = 'a i M.t
end = struct
  type 'a i = 'a
  type +'a t = 'a i M.t
end

module Positive_ref = Make_covariant(struct type 'a t = 'a ref end)
[%%expect {|
Line 6, characters 2-23:
6 |   type +'a t = 'a i M.t
      ^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is invariant.
|}]
