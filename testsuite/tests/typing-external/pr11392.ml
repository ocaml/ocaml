(* TEST
   * expect
*)

type 'self nat =
  | Z
  | S of 'self
;;
[%%expect{|
type 'self nat = Z | S of 'self
|}]



(* without rectypes: rejected *)
external cast : int -> 'self nat as 'self = "%identity"
;;
[%%expect{|
Line 1, characters 16-41:
1 | external cast : int -> 'self nat as 'self = "%identity"
                    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This alias is bound to type int -> 'a nat
       but is used as an instance of type 'a
       The type variable 'a occurs inside int -> 'a nat
|}]

#rectypes;;

(* with rectypes: should be accepted, but crashes *)
external cast : int -> 'self nat as 'self = "%identity"
;;
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 1341, characters 43-49: Assertion failed

|}]
