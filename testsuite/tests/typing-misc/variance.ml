(* TEST
 flags = "-i-variance";
 expect;
*)

(* Syntax *)
type +'a t;;
type -'a t;;
type +-'a t;;
type -+'a t;;
type + 'a t;;
type - 'a t;;
type +- 'a t;;
type -+ 'a t;;
[%%expect{|
type +'a t
type -'a t
type +-'a t
type +-'a t
type +'a t
type -'a t
type +-'a t
type +-'a t
|}]
(* Expect doesn't support syntax errors
type + - 'a t
[%%expect]
*)

(* #8698 *)

(* Actually, this is not a bug *)
type +'a t = [> `Foo of 'a -> unit] as 'a;;
[%%expect{|
type !'a t = 'a constraint 'a = [> `Foo of 'a -> unit ]
|}, Principal{|
type +!'a t = 'a constraint 'a = [> `Foo of 'a -> unit ]
|}]

(* strengthening *)

type 'a t = (('a -> unit) -> unit);;
let tl = !(ref ([] : 'a t list));;
[%%expect{|
type +!'a t = ('a -> unit) -> unit
val tl : '_a t list = []
|}]

type 'a u = U of (('a -> unit) -> unit);;
let ul = !(ref ([] : 'a u list));;
[%%expect{|
type +!'a u = U of (('a -> unit) -> unit)
val ul : 'a u list = []
|}]

(* #11869 *)

module type s = sig type t end;;
type !'a t = (module s with type t = 'a);;
[%%expect{|
module type s = sig type t end
type !'a t = (module s with type t = 'a)
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

module S = struct
  module Make_covariant(M: sig type 'a t end): sig
    type 'a i = 'a
    type +'a t = 'a i M.t
  end = struct
    type 'a i = 'a
    type +'a t = 'a i M.t
  end

  module Positive_ref = Make_covariant(struct type 'a t = 'a ref end)
end;;
[%%expect {|
Line 7, characters 4-25:
7 |     type +'a t = 'a i M.t
        ^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is invariant.
|}]


(* #14200 *)

module M : sig
  type 'a t = private string
  type foo = private int
  type bar = private int
  val foo : foo t
end = struct
  type 'a t = string
  type foo = private int
  type bar = private int
  let foo = "foo"
end;;
[%%expect{|
module M :
  sig
    type 'a t = private string
    type foo = private int
    type bar = private int
    val foo : foo t
  end
|}]

module Coerce : sig
  type +'a pos = private string
  type -'a neg = private string

  val p : 'a M.t -> 'a pos
  val pn : 'a pos -> 'a neg
  val n : 'a neg -> 'a M.t
end = struct
  type 'a pos = 'a M.t
  type 'a neg = 'a M.t

  let p = Fun.id
  let pn = Fun.id
  let n = Fun.id
end

let bar : M.bar M.t =
  M.foo
  |> Coerce.p
  |> (fun t -> (t :> int Coerce.pos))
  |> Coerce.pn
  |> (fun t -> (t :> M.bar Coerce.neg))
  |> Coerce.n ;;
[%%expect{|
Lines 8-15, characters 6-3:
 8 | ......struct
 9 |   type 'a pos = 'a M.t
10 |   type 'a neg = 'a M.t
11 |
12 |   let p = Fun.id
13 |   let pn = Fun.id
14 |   let n = Fun.id
15 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type 'a pos = 'a M.t
           type 'a neg = 'a M.t
           val p : 'a -> 'a
           val pn : 'a -> 'a
           val n : 'a -> 'a
         end
       is not included in
         sig
           type +'a pos = private string
           type -'a neg = private string
           val p : 'a M.t -> 'a pos
           val pn : 'a pos -> 'a neg
           val n : 'a neg -> 'a M.t
         end
       Type declarations do not match:
         type 'a pos = 'a M.t
       is not included in
         type +'a pos = private string
       Their variances do not agree.
Unexecuted phrases: 1 phrases did not execute due to an error
|}]

type 'a priv = private int

module Bad : sig
  type +-'a t = private int
  val inj : 'a priv -> 'a t
  val prj : 'a t -> 'a priv
end = struct
  type 'a t = 'a priv
  let inj = Fun.id
  let prj = Fun.id
end
let cast x = x |> Bad.inj |> (fun x -> (x :> _ M.t)) |> Bad.prj;;
[%%expect{|
type 'a priv = private int
Lines 7-11, characters 6-3:
 7 | ......struct
 8 |   type 'a t = 'a priv
 9 |   let inj = Fun.id
10 |   let prj = Fun.id
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a priv val inj : 'a -> 'a val prj : 'a -> 'a end
       is not included in
         sig
           type +-'a t = private int
           val inj : 'a priv -> 'a t
           val prj : 'a t -> 'a priv
         end
       Type declarations do not match:
         type 'a t = 'a priv
       is not included in
         type +-'a t = private int
       Their variances do not agree.
Unexecuted phrases: 1 phrases did not execute due to an error
|}]
