(* TEST
flags = " -rectypes "
   * expect
*)

module type S = sig
  type t
end;;

[%%expect{|
module type S = sig type t end
|}]

(* Recursive functors resolve. *)

let f (x : (({M : S} -> M.t * ({N : S} -> N.t * 'a))) as 'a) =
  (x : (({O : S} -> O.t * 'b) as 'b));;

[%%expect{|
val f : ({O : S} -> O.t * 'a as 'a) -> 'a = <fun>
|}, Principal{|
val f :
  ({M : S} -> M.t * ({N : S} -> N.t * 'a) as 'a) ->
  ({O : S} -> O.t * 'b as 'b) = <fun>
|}]

let f (x : (({A : S} -> A.t * ({B : S} -> B.t * ({C : S} -> C.t * 'a)))) as 'a) =
  (x : (({M : S} -> M.t * ({N : S} -> N.t * 'b))) as 'b);;

[%%expect{|
val f : ({N : S} -> N.t * 'a as 'a) -> 'a = <fun>
|}, Principal{|
val f :
  ({A : S} -> A.t * ({B : S} -> B.t * ({C : S} -> C.t * 'a)) as 'a) ->
  ({M : S} -> M.t * ({N : S} -> N.t * 'b) as 'b) = <fun>
|}]

(* Recursive functors do not result in module name aliasing. *)

let f (x : (({X : S} -> X.t * ({Y : S} -> X.t * 'a)) as 'a)) =
  (x  : ({Z : S} -> Z.t * 'a));;

[%%expect{|
Line 2, characters 3-4:
2 |   (x  : ({Z : S} -> Z.t * 'a));;
       ^
Error: This expression has type
         {X/1 : S} -> X/1.t * ({Y : S} -> X/1.t * 'a as 'b) as 'a
       but an expression was expected of type
         {Z : S} -> Z.t * ({X/2 : S} -> X/2.t * 'b)
       Type X/1.t is not compatible with type X/2.t
|}, Principal{|
Line 2, characters 3-4:
2 |   (x  : ({Z : S} -> Z.t * 'a));;
       ^
Error: This expression has type
         {X/3 : S} -> X/3.t * ({Y : S} -> X/3.t * 'a) as 'a
       but an expression was expected of type
         {Z : S} ->
         Z.t *
         ({X/1 : S} ->
          X/1.t * ({Y : S} -> X/2.t * ({X : S} -> X.t * 'b) as 'b))
       Type X/3.t is not compatible with type X/1.t
|}]
