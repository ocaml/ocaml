(* TEST
flags = "-dlambda -dno-locations -dno-unique-ids";
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

let curried : type a. a rep -> a -> float = fun Float a -> a +. 1.
[%%expect {|
(let
  (curried =
     (function param[int] a : float
       (if param (raise (makeblock 0 (global Match_failure!) [0: "" 1 48]))
         (+. a 1.))))
  (apply (field_mut 1 (global Toploop!)) "curried" curried))
val curried : 'a rep -> 'a -> float = <fun>
|}]

let curried_ok : type a. (a,float) Type.eq -> a -> float = fun Type.Equal a -> a +. 1.
[%%expect {|
(let (curried_ok = (function param[int] a[float] : float (+. a 1.)))
  (apply (field_mut 1 (global Toploop!)) "curried_ok" curried_ok))
val curried_ok : ('a, float) Type.eq -> 'a -> float = <fun>
|}]


let curried_first : type a b. unit list -> (a,float) Type.eq -> b rep -> a -> b -> float =
  fun [] Type.Equal Float a b -> a +. b
[%%expect {|
(let
  (curried_first =
     (function param param[int] param[int] a b : float
       (if param (raise (makeblock 0 (global Match_failure!) [0: "" 2 6]))
         (if param (raise (makeblock 0 (global Match_failure!) [0: "" 2 20]))
           (+. a b)))))
  (apply (field_mut 1 (global Toploop!)) "curried_first" curried_first))
val curried_first :
  unit list -> ('a, float) Type.eq -> 'b rep -> 'a -> 'b -> float = <fun>
|}]
