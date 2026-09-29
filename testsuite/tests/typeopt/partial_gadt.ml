(* TEST
flags = "-dlambda -dno-locations -dcanonical-ids";
expect;
*)

(* Test that we do not use local equation introduced by partial matching when computing
   the value kinds of function parameters *)

[@@@warning "-8"]

type 'a rep = Float : float rep | Int : int rep
[%%expect {|
0
0
type 'a rep = Float : float rep | Int : int rep
|}]

(** Expected kind [int -> any -> float]:
    Since the match on [Float] can fail, one cannot use the equation [a = Float]
    to infer the kind [float] for [a].
*)
let curried : type a. a rep -> a -> float = fun Float a -> a +. 1.
[%%expect {|
(let
  (curried/0 =
     (function param/0[int] a/0 : float
       (if param/0
         (raise (makeblock 0 (global Match_failure/0!) [0: "" 1 48]))
         (+. a/0 1.))))
  (apply (field_mut 1 (global Toploop!)) "curried" curried/0))
val curried : 'a rep -> 'a -> float = <fun>
|}]

(** Expected kind [any -> any -> float]:
    The [Type.Equal] pattern is total, however the default expression
    can raise and thus the pattern match in
    {[
      match assert false with
      | Type.Equal -> ...
    ]}
    fails to account for the exception case.
    Thus once again, we cannot use the equation [a = Float] to infer the kind
    [float] for [a].
*)
let opt (type a) ?p:(Type.Equal:(a,float) Type.eq=assert false) (x:a) = 1. +. x
[%%expect {|
(let
  (opt/0 =
     (function *opt*/0 x/0 : float
       (let
         (*match*/0 =
            (if *opt*/0 (field_imm 0 *opt*/0)
              (raise (makeblock 0 (global Assert_failure/0!) [0: "" 1 50]))))
         (+. 1. x/0))))
  (apply (field_mut 1 (global Toploop!)) "opt" opt/0))
val opt : ?p:('a, float) Type.eq -> 'a -> float = <fun>
|}]

(** Expected kind: [int -> float -> float]
    [Type.Equal] is an irrefutable pattern, we can safely use [a=float] to infer
    [a:float].
*)
let curried_ok : type a. (a,float) Type.eq -> a -> float = fun Type.Equal a -> a +. 1.
[%%expect {|
(let (curried_ok/0 = (function param/1[int] a/1[float] : float (+. a/1 1.)))
  (apply (field_mut 1 (global Toploop!)) "curried_ok" curried_ok/0))
val curried_ok : ('a, float) Type.eq -> 'a -> float = <fun>
|}]


(** Expected kind: [any -> int -> int -> any -> any -> float],
    valid optimization [any -> int -> int -> float -> any -> float].
    The empty list pattern [[]] is an refutable pattern, we are currently refusing
    to use any GADTs equation after this pattern. However, the equation [a=float]
    is introduced by an irrefutable pattern without any dependencies on a refutable
    pattern. We could thus theoretically use this equation to refine the inferred
    kind for the function to [any -> int -> int -> float -> any].
    A good first approximation to implement this refinement would be to only freeze
    the set of GADTs equation when we observe a refutable pattern argument which
    adds a local equation.
*)
let curried_first : type a b. unit list -> (a,float) Type.eq -> b rep -> a -> b -> float =
  fun [] Type.Equal Float a b -> a +. b
[%%expect {|
(let
  (curried_first/0 =
     (function param/2 param/3[int] param/4[int] a/2 b/0 : float
       (if param/2
         (raise (makeblock 0 (global Match_failure/0!) [0: "" 2 6]))
         (if param/4
           (raise (makeblock 0 (global Match_failure/0!) [0: "" 2 20]))
           (+. a/2 b/0)))))
  (apply (field_mut 1 (global Toploop!)) "curried_first" curried_first/0))
val curried_first :
  unit list -> ('a, float) Type.eq -> 'b rep -> 'a -> 'b -> float = <fun>
|}]
