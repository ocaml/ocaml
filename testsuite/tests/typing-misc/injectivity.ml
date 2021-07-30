(* TEST
   * expect
*)

(* Syntax *)

type ! 'a t = private 'a ref
type +! 'a t = private 'a
type -!'a t = private 'a -> unit
type + !'a t = private 'a
type - ! 'a t = private 'a -> unit
type !+ 'a t = private 'a
type !-'a t = private 'a -> unit
type ! +'a t = private 'a
type ! -'a t = private 'a -> unit
[%%expect{|
type 'a t = private 'a ref
type +'a t = private 'a
type -'a t = private 'a -> unit
type +'a t = private 'a
type -'a t = private 'a -> unit
type +'a t = private 'a
type -'a t = private 'a -> unit
type +'a t = private 'a
type -'a t = private 'a -> unit
|}]
(* Expect doesn't support syntax errors
type -+ 'a t
[%%expect]
type -!! 'a t
[%%expect]
*)

(* Define an injective abstract type, and use it in a GADT
   and a constrained type *)
module M : sig type +!'a t end = struct type 'a t = 'a list end
[%%expect{|
module M : sig type +!'a t end
|}]
type _ t = M : 'a -> 'a M.t t (* OK *)
type 'a u = 'b constraint 'a = 'b M.t
[%%expect{|
type _ t = M : 'a -> 'a M.t t
type 'a u = 'b constraint 'a = 'b M.t
|}]

(* Without the injectivity annotation, the cannot be defined *)
module N : sig type +'a t end = struct type 'a t = 'a list end
[%%expect{|
module N : sig type +'a t end
|}]
type _ t = N : 'a -> 'a N.t t (* KO *)
[%%expect{|
Line 1, characters 0-29:
1 | type _ t = N : 'a -> 'a N.t t (* KO *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, a type variable cannot be deduced
       from the type parameters.
|}]
type 'a u = 'b constraint 'a = 'b N.t
[%%expect{|
Line 1, characters 0-37:
1 | type 'a u = 'b constraint 'a = 'b N.t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, a type variable cannot be deduced
       from the type parameters.
|}]

(* Of course, the internal type should be injective in this parameter *)
module M : sig type +!'a t end = struct type 'a t = int end (* KO *)
[%%expect{|
Line 1, characters 33-59:
1 | module M : sig type +!'a t end = struct type 'a t = int end (* KO *)
                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = int end
       is not included in
         sig type +!'a t end
       Type declarations do not match:
         type 'a t = int
       is not included in
         type +!'a t
       Their variances do not agree.
|}]

(* Annotations in type abbreviations allow to check injectivity *)
type !'a t = 'a list
type !'a u = int
[%%expect{|
type 'a t = 'a list
Line 2, characters 0-16:
2 | type !'a u = int
    ^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be injective invariant,
       but it is unrestricted.
|}]
type !'a t = private 'a list
type !'a t = private int
[%%expect{|
type 'a t = private 'a list
Line 2, characters 0-24:
2 | type !'a t = private int
    ^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be injective invariant,
       but it is unrestricted.
|}]

(* Can also use to add injectivity in private row types *)
module M : sig type !'a t = private < m : int ; .. > end =
  struct type 'a t = < m : int ; n : 'a > end
type 'a u = M : 'a -> 'a M.t u
[%%expect{|
module M : sig type !'a t = private < m : int; .. > end
type 'a u = M : 'a -> 'a M.t u
|}]
module M : sig type 'a t = private < m : int ; .. > end =
  struct type 'a t = < m : int ; n : 'a > end
type 'a u = M : 'a -> 'a M.t u
[%%expect{|
module M : sig type 'a t = private < m : int; .. > end
Line 3, characters 0-30:
3 | type 'a u = M : 'a -> 'a M.t u
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, a type variable cannot be deduced
       from the type parameters.
|}]
module M : sig type !'a t = private < m : int ; .. > end =
  struct type 'a t = < m : int > end
[%%expect{|
Line 2, characters 2-36:
2 |   struct type 'a t = < m : int > end
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = < m : int > end
       is not included in
         sig type !'a t = private < m : int; .. > end
       Type declarations do not match:
         type 'a t = < m : int >
       is not included in
         type !'a t
       Their variances do not agree.
|}]

(* Injectivity annotations are inferred correctly for constrained parameters *)
type 'a t = 'b constraint 'a = <b:'b>
type !'b u = <b:'b> t
[%%expect{|
type 'a t = 'b constraint 'a = < b : 'b >
type 'b u = < b : 'b > t
|}]

(* Ignore injectivity for nominal types *)
type !_ t = X
[%%expect{|
type _ t = X
|}]

(* Beware of constrained parameters *)
type (_,_) eq = Refl : ('a,'a) eq
type !'a t = private 'b constraint 'a = < b : 'b > (* OK *)
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
type 'a t = private 'b constraint 'a = < b : 'b >
|}]

type !'a t = private 'b constraint 'a = < b : 'b; c : 'c > (* KO *)
module M : sig type !'a t constraint 'a = < b : 'b; c : 'c > end =
  struct type nonrec 'a t = 'a t end
let inj_t : type a b. (<b:_; c:a> M.t, <b:_; c:b> M.t) eq -> (a, b) eq =
  fun Refl -> Refl
[%%expect{|
Line 1, characters 0-58:
1 | type !'a t = private 'b constraint 'a = < b : 'b; c : 'c > (* KO *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be injective invariant,
       but it is unrestricted.
|}]

(* One cannot assume that abstract types are not injective *)
module F(X : sig type 'a t end) = struct
  type 'a u = unit constraint 'a = 'b X.t
  type _ x = G : 'a -> 'a u x
end
module M = F(struct type 'a t = 'a end)
let M.G (x : bool) = M.G 3
[%%expect{|
Line 3, characters 2-29:
3 |   type _ x = G : 'a -> 'a u x
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, a type variable cannot be deduced
       from the type parameters.
|}]

(* Try to be clever *)
type 'a t = unit
type !'a u = int constraint 'a = 'b t
[%%expect{|
type 'a t = unit
type 'a u = int constraint 'a = 'b t
|}]
module F(X : sig type 'a t end) = struct
  type !'a u = 'b constraint 'a = <b : 'b> constraint 'b = _ X.t
end
[%%expect{|
module F :
  functor (X : sig type 'a t end) ->
    sig type 'a u = 'b X.t constraint 'a = < b : 'b X.t > end
|}]
(* But not too clever *)
module F(X : sig type 'a t end) = struct
  type !'a u = 'b X.t constraint 'a = <b : 'b X.t>
end
[%%expect{|
Line 2, characters 2-50:
2 |   type !'a u = 'b X.t constraint 'a = <b : 'b X.t>
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be injective invariant,
       but it is unrestricted.
|}]
module F(X : sig type 'a t end) = struct
  type !'a u = 'b constraint 'a = <b : _ X.t as 'b>
end
[%%expect{|
module F :
  functor (X : sig type 'a t end) ->
    sig type 'a u = 'b X.t constraint 'a = < b : 'b X.t > end
|}, Principal{|
Line 2, characters 2-51:
2 |   type !'a u = 'b constraint 'a = <b : _ X.t as 'b>
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be injective invariant,
       but it is unrestricted.
|}]

(* Motivating examples with GADTs *)

type (_,_) eq = Refl : ('a,'a) eq

module Vec : sig
  type +!'a t
  val make : int -> (int -> 'a) -> 'a t
  val get : 'a t -> int -> 'a
end = struct
  type 'a t = Vec of Obj.t array
  let make n f = Vec (Obj.magic Array.init n f)
  let get (Vec v) n = Obj.obj (Array.get v n)
end

type _ ty =
  | Int : int ty
  | Fun : 'a ty * 'b ty -> ('a -> 'b) ty
  | Vec : 'a ty -> 'a Vec.t ty

type dyn = Dyn : 'a ty * 'a -> dyn

let rec eq_ty : type a b. a ty -> b ty -> (a,b) eq option =
  fun t1 t2 -> match t1, t2 with
  | Int, Int -> Some Refl
  | Fun (t11, t12), Fun (t21, t22) ->
      begin match eq_ty t11 t21, eq_ty t12 t22 with
      | Some Refl, Some Refl -> Some Refl
      | _ -> None
      end
  | Vec t1, Vec t2 ->
      begin match eq_ty t1 t2 with
      | Some Refl -> Some Refl
      | None -> None
      end
  | _ -> None

let undyn : type a. a ty -> dyn -> a option =
  fun t1 (Dyn (t2, v)) ->
    match eq_ty t1 t2 with
    | Some Refl -> Some v
    | None -> None

let v = Vec.make 3 (fun n -> Vec.make n (fun m -> (m*n)))

let int_vec_vec = Vec (Vec Int)

let d = Dyn (int_vec_vec, v)

let Some v' = undyn int_vec_vec d
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
module Vec :
  sig
    type +!'a t
    val make : int -> (int -> 'a) -> 'a t
    val get : 'a t -> int -> 'a
  end
type _ ty =
    Int : int ty
  | Fun : 'a ty * 'b ty -> ('a -> 'b) ty
  | Vec : 'a ty -> 'a Vec.t ty
type dyn = Dyn : 'a ty * 'a -> dyn
val eq_ty : 'a ty -> 'b ty -> ('a, 'b) eq option = <fun>
val undyn : 'a ty -> dyn -> 'a option = <fun>
val v : int Vec.t Vec.t = <abstr>
val int_vec_vec : int Vec.t Vec.t ty = Vec (Vec Int)
val d : dyn = Dyn (Vec (Vec Int), <poly>)
Line 47, characters 4-11:
47 | let Some v' = undyn int_vec_vec d
         ^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
None
val v' : int Vec.t Vec.t = <abstr>
|}]

(* Break it (using magic) *)
module Vec : sig
  type +!'a t
  val eqt : ('a t, 'b t) eq
end = struct
  type 'a t = 'a
  let eqt = Obj.magic Refl (* Never do that! *)
end

type _ ty =
  | Int : int ty
  | Vec : 'a ty -> 'a Vec.t ty

let coe : type a b. (a,b) eq -> a ty -> b ty =
  fun Refl x -> x
let eq_int_any : type a.  unit -> (int, a) eq = fun () ->
  let vec_ty : a Vec.t ty = coe Vec.eqt (Vec Int) in
  let Vec Int = vec_ty in Refl
[%%expect{|
module Vec : sig type +!'a t val eqt : ('a t, 'b t) eq end
type _ ty = Int : int ty | Vec : 'a ty -> 'a Vec.t ty
val coe : ('a, 'b) eq -> 'a ty -> 'b ty = <fun>
Line 17, characters 2-30:
17 |   let Vec Int = vec_ty in Refl
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Vec (Vec Int)
val eq_int_any : unit -> (int, 'a) eq = <fun>
|}]

(* Not directly related: injectivity and constraints *)
type 'a t = 'b constraint 'a = <b : 'b>
class type ['a] ct = object method m : 'b constraint 'a = < b : 'b > end
[%%expect{|
type 'a t = 'b constraint 'a = < b : 'b >
class type ['a] ct = object constraint 'a = < b : 'b > method m : 'b end
|}]

type _ u = M : 'a -> 'a t u (* OK *)
[%%expect{|
type _ u = M : < b : 'a > -> < b : 'a > t u
|}]
type _ v = M : 'a -> 'a ct v (* OK *)
[%%expect{|
type _ v = M : < b : 'a > -> < b : 'a > ct v
|}]

type 'a t = 'b constraint 'a = <b : 'b; c : 'c>
type _ u = M : 'a -> 'a t u (* KO *)
[%%expect{|
type 'a t = 'b constraint 'a = < b : 'b; c : 'c >
Line 2, characters 0-27:
2 | type _ u = M : 'a -> 'a t u (* KO *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, a type variable cannot be deduced
       from the type parameters.
|}]


(* #9721 by Jeremy Yallop *)

(* First, some standard bits and pieces for equality & injectivity: *)

type (_,_) eql = Refl : ('a, 'a) eql

module Uninj(X: sig type !'a t end) :
sig val uninj : ('a X.t, 'b X.t) eql -> ('a, 'b) eql end =
struct let uninj : type a b. (a X.t, b X.t) eql -> (a, b) eql = fun Refl -> Refl end

let coerce : type a b. (a, b) eql -> a -> b = fun Refl x -> x;;
[%%expect{|
type (_, _) eql = Refl : ('a, 'a) eql
module Uninj :
  functor (X : sig type !'a t end) ->
    sig val uninj : ('a X.t, 'b X.t) eql -> ('a, 'b) eql end
val coerce : ('a, 'b) eql -> 'a -> 'b = <fun>
|}]

(* Now the questionable part, defining two "injective" type definitions in
   a pair of mutually-recursive modules. These definitions are correctly
   rejected if given as a pair of mutually-recursive types, but wrongly
   accepted when defined as follows:
*)

module rec R : sig type !'a t = [ `A of 'a S.t] end = R
       and S : sig type !'a t = 'a R.t end = S ;;
[%%expect{|
Line 1, characters 19-47:
1 | module rec R : sig type !'a t = [ `A of 'a S.t] end = R
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be injective invariant,
       but it is invariant.
|}]

(* The parameter of R.t is never used, so we can build an equality witness
   for any instantiation: *)

let x_eq_y : (int R.t, string R.t) eql = Refl
let boom = let module U = Uninj(R) in print_endline (coerce (U.uninj x_eq_y) 0)
;;
[%%expect{|
Line 1, characters 18-21:
1 | let x_eq_y : (int R.t, string R.t) eql = Refl
                      ^^^
Error: Unbound module R
|}]

(* #10028 by Stephen Dolan *)

module rec A : sig
  type _ t = Foo : 'a -> 'a A.s t
  type 'a s = T of 'a
end =
  A
;;
[%%expect{|
module rec A : sig type _ t = Foo : 'a -> 'a A.s t type 'a s = T of 'a end
|}]
