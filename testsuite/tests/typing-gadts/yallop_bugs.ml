(* TEST
   * expect
*)

(* Injectivity *)

type (_, _) eq = Refl : ('a, 'a) eq

let magic : 'a 'b. 'a -> 'b =
  fun (type a b) (x : a) ->
    let module M =
      (functor (T : sig type 'a t end) ->
       struct
         let f (Refl : (a T.t, b T.t) eq) = (x :> b)
       end)
        (struct type 'a t = unit end)
    in M.f Refl
;;
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
Line 8, characters 44-52:
8 |          let f (Refl : (a T.t, b T.t) eq) = (x :> b)
                                                ^^^^^^^^
Error: Type a is not a subtype of b
|}];;

(* Variance and subtyping *)

type (_, +_) eq = Refl : ('a, 'a) eq

let magic : 'a 'b. 'a -> 'b =
  fun (type a) (type b) (x : a) ->
    let bad_proof (type a) =
      (Refl : (< m : a>, <m : a>) eq :> (<m : a>, < >) eq) in
    let downcast : type a. (a, < >) eq -> < > -> a =
      fun (type a) (Refl : (a, < >) eq) (s : < >) -> (s :> a) in
    (downcast bad_proof ((object method m = x end) :> < >)) # m
;;
[%%expect{|
Line 1, characters 0-36:
1 | type (_, +_) eq = Refl : ('a, 'a) eq
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this GADT definition, the variance of some parameter
       cannot be checked
|}];;

(* Record patterns *)

type _ t =
  | IntLit : int t
  | BoolLit : bool t

let check : type s . s t * s -> bool = function
  | BoolLit, false -> false
  | IntLit , 6 -> false
;;
[%%expect{|
type _ t = IntLit : int t | BoolLit : bool t
Lines 5-7, characters 39-23:
5 | .......................................function
6 |   | BoolLit, false -> false
7 |   | IntLit , 6 -> false
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(BoolLit, true)
val check : 's t * 's -> bool = <fun>
|}];;

type ('a, 'b) pair = { fst : 'a; snd : 'b }

let check : type s . (s t, s) pair -> bool = function
  | {fst = BoolLit; snd = false} -> false
  | {fst = IntLit ; snd =  6} -> false
;;
[%%expect{|
type ('a, 'b) pair = { fst : 'a; snd : 'b; }
Lines 3-5, characters 45-38:
3 | .............................................function
4 |   | {fst = BoolLit; snd = false} -> false
5 |   | {fst = IntLit ; snd =  6} -> false
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{fst=BoolLit; snd=true}
val check : ('s t, 's) pair -> bool = <fun>
|}];;
