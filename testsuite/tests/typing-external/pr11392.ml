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

(* with rectypes: accepted (used to crash) *)
external cast : int -> 'self nat as 'self = "%identity"
;;
[%%expect{|
external cast : int -> 'a nat as 'a = "%identity"
|}]
