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
Line _, characters 29-30:
    | Refl -> if b then x else y
                               ^
Error: This expression has type b = a but an expression was expected of type
         a
       This instance of a is ambiguous:
       it would escape the scope of its equation
|}]

let ret_e2 (type a b) (b : bool) (wit : (a, b) eq) (x : a) (y : b) =
  match wit with
  | Refl -> if b then x else y
  | _ -> y
;;
[%%expect{|
Line _, characters 29-30:
    | Refl -> if b then x else y
                               ^
Error: This expression has type b = a but an expression was expected of type
         a
       This instance of a is ambiguous:
       it would escape the scope of its equation
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
Line _, characters 16-17:
    | Refl -> [x; y]
                  ^
Error: This expression has type b = a but an expression was expected of type
         a
       This instance of a is ambiguous:
       it would escape the scope of its equation
|}]

let ret_g1 (type a b) (wit : (a, b) eq) (x : a) (y : b) =
  match wit with
  | Refl -> [x; y]
  | _ -> [y]
;;
[%%expect{|
Line _, characters 16-17:
    | Refl -> [x; y]
                  ^
Error: This expression has type b = a but an expression was expected of type
         a
       This instance of a is ambiguous:
       it would escape the scope of its equation
|}]
