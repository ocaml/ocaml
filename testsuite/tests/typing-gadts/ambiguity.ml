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

(* First reported in MPR#7617: the typechecker arbitrarily picks a
   representative for an ambivalent type escaping its scope.
   The commit that was implemented poses problems of its own: we are now
   unifying the type of the patterns in the environment of each pattern, instead
   of the outter one. The code discussed in PR#7617 passes because each branch
   contains the same equation, but consider the following cases: *)

let f (type a b) (x : (a, b) eq) =
  match x, [] with
  | Refl, [(_ : a) | (_ : b)] -> []
  | _, [(_ : a)] -> []
;;
[%%expect{|
Line _, characters 4-29:
    | Refl, [(_ : a) | (_ : b)] -> []
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type (a, b) eq * b list
       but a pattern was expected which matches values of type 'a
       This instance of b is ambiguous:
       it would escape the scope of its equation
|}]

let g1 (type a b) (x : (a, b) eq) =
  match x, [] with
  | Refl, [(_ : a) | (_ : b)] -> []
  | _, [(_ : b)] -> []
;;
[%%expect{|
Line _, characters 4-29:
    | Refl, [(_ : a) | (_ : b)] -> []
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type (a, b) eq * b list
       but a pattern was expected which matches values of type 'a
       This instance of b is ambiguous:
       it would escape the scope of its equation
|}]

let g2 (type a b) (x : (a, b) eq) =
  match x, [] with
  | Refl, [(_ : b) | (_ : a)] -> []
  | _, [(_ : a)] -> []
;;
[%%expect{|
Line _, characters 4-29:
    | Refl, [(_ : b) | (_ : a)] -> []
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type (a, b) eq * b list
       but a pattern was expected which matches values of type 'a
       This instance of b is ambiguous:
       it would escape the scope of its equation
|}]

let h1 (type a b) (x : (a, b) eq) =
  match x, [] with
  | _, [(_ : a)] -> []
  | Refl, [(_ : a) | (_ : b)] -> []
;;
[%%expect{|
Line _, characters 4-29:
    | Refl, [(_ : a) | (_ : b)] -> []
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type (a, b) eq * b list
       but a pattern was expected which matches values of type 'a
       This instance of b is ambiguous:
       it would escape the scope of its equation
|}]

let h2 (type a b) (x : (a, b) eq) =
  match x, [] with
  | _, [(_ : b)] -> []
  | Refl, [(_ : a) | (_ : b)] -> []
;;
[%%expect{|
Line _, characters 4-29:
    | Refl, [(_ : a) | (_ : b)] -> []
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type (a, b) eq * b list
       but a pattern was expected which matches values of type 'a
       This instance of b is ambiguous:
       it would escape the scope of its equation
|}]

let h3 (type a b) (x : (a, b) eq) =
  match x, [] with
  | _, [(_ : a)] -> []
  | Refl, [(_ : b) | (_ : a)] -> []
;;
[%%expect{|
Line _, characters 4-29:
    | Refl, [(_ : b) | (_ : a)] -> []
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type (a, b) eq * b list
       but a pattern was expected which matches values of type 'a
       This instance of b is ambiguous:
       it would escape the scope of its equation
|}]
