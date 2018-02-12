(* TEST
   * expect
*)

[@@@warning "-8-11-12"] (* reduce the noise. *)

type ('a, 'b) eq = Refl : ('a, 'a) eq;;
[%%expect{|
type ('a, 'b) eq = Refl : ('a, 'a) eq
|}];;

let ret_e1 (type a b) (b : bool) (wit : (a, b) eq) (x : a) (y : b) =
  match wit with
  | Refl -> if b then x else y
  | _ -> x
;;
[%%expect{|
val ret_e1 : bool -> ('a, 'b) eq -> 'a -> 'b -> 'a = <fun>
|}]

let ret_e2 (type a b) (b : bool) (wit : (a, b) eq) (x : a) (y : b) =
  match wit with
  | Refl -> if b then x else y
  | _ -> y
;;
[%%expect{|
Line _, characters 9-10:
    | _ -> y
           ^
Error: This expression has type b but an expression was expected of type a
|}]

let ret_ei1 (type a) (b : bool) (wit : (a, int) eq) (x : a) =
  match wit with
  | Refl -> if b then x else 0
  | _ -> x
;;
[%%expect{|
Line _, characters 29-30:
    | Refl -> if b then x else 0
                               ^
Error: This expression has type int but an expression was expected of type
         a = int
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}]

let ret_ei2 (type a) (b : bool) (wit : (a, int) eq) (x : a) =
  match wit with
  | Refl -> if b then x else 0
  | _ -> x
;;
[%%expect{|
Line _, characters 29-30:
    | Refl -> if b then x else 0
                               ^
Error: This expression has type int but an expression was expected of type
         a = int
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}]


let ret_f (type a b) (wit : (a, b) eq) (x : a) (y : b) =
  match wit with
  | Refl -> [x; y]
  | _ -> [x]
;;
[%%expect{|
val ret_f : ('a, 'b) eq -> 'a -> 'b -> 'a list = <fun>
|}]

let ret_g1 (type a b) (wit : (a, b) eq) (x : a) (y : b) =
  match wit with
  | Refl -> [x; y]
  | _ -> [y]
;;
[%%expect{|
Line _, characters 10-11:
    | _ -> [y]
            ^
Error: This expression has type b but an expression was expected of type a
|}, Principal{|
Line _, characters 9-12:
    | _ -> [y]
           ^^^
Error: This expression has type b list but an expression was expected of type
         a list
       Type b is not compatible with type a
|}]
