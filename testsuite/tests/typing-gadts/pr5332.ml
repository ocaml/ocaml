(* TEST
   * expect
*)

type ('env, 'a) var =
 | Zero : ('a * 'env, 'a) var
 | Succ : ('env, 'a) var -> ('b * 'env, 'a) var
;;
type ('env, 'a) typ =
 | Tint : ('env, int) typ
 | Tbool : ('env, bool) typ
 | Tvar : ('env, 'a) var -> ('env, 'a) typ
;;
let f : type env a. (env, a) typ -> (env, a) typ -> int = fun ta tb ->
 match ta, tb with
   | Tint, Tint -> 0
   | Tbool, Tbool -> 1
   | Tvar var, tb -> 2
   | _ -> .   (* error *)
;;
[%%expect{|
type ('env, 'a) var =
    Zero : ('a * 'env, 'a) var
  | Succ : ('env, 'a) var -> ('b * 'env, 'a) var
type ('env, 'a) typ =
    Tint : ('env, int) typ
  | Tbool : ('env, bool) typ
  | Tvar : ('env, 'a) var -> ('env, 'a) typ
Line 15, characters 5-6:
     | _ -> .   (* error *)
       ^
Error:
This match case could not be refuted.
Here is an example of a value that would reach it: (Tint, Tvar Zero)
|}];;
(* let x = f Tint (Tvar Zero) ;; *)
