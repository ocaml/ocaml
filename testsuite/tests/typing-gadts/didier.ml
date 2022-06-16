(* TEST
   * expect
*)

type 'a ty =
  | Int : int ty
  | Bool : bool ty

let fbool (type t) (x : t) (tag : t ty) =
  match tag with
  | Bool -> x
;;
[%%expect{|
type 'a ty = Int : int ty | Bool : bool ty
Lines 6-7, characters 2-13:
6 | ..match tag with
7 |   | Bool -> x
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Int
val fbool : 't -> 't ty -> 't = <fun>
|}];;
(* val fbool : 'a -> 'a ty -> 'a = <fun> *)
(** OK: the return value is x of type t **)

let fint (type t) (x : t) (tag : t ty) =
  match tag with
  | Int -> x > 0
;;
[%%expect{|
Lines 2-3, characters 2-16:
2 | ..match tag with
3 |   | Int -> x > 0
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Bool
val fint : 't -> 't ty -> bool = <fun>
|}];;
(* val fint : 'a -> 'a ty -> bool = <fun> *)
(** OK: the return value is x > 0 of type bool;
This has used the equation t = bool, not visible in the return type **)

(* not principal *)
let f (type t) (x : t) (tag : t ty) =
  match tag with
  | Int -> x > 0
  | Bool -> x
;;
[%%expect{|
val f : 't -> 't ty -> bool = <fun>
|}, Principal{|
Line 4, characters 12-13:
4 |   | Bool -> x
                ^
Error: This expression has type t = bool
       but an expression was expected of type bool
       This instance of bool is ambiguous:
       it would escape the scope of its equation
|}];;
(* val f : 'a -> 'a ty -> bool = <fun> *)

(* fail for both *)
let g (type t) (x : t) (tag : t ty) =
  match tag with
  | Bool -> x
  | Int -> x > 0
;;
[%%expect{|
Line 4, characters 11-16:
4 |   | Int -> x > 0
               ^^^^^
Error: This expression has type bool but an expression was expected of type
         t = int
|}, Principal{|
Line 4, characters 11-16:
4 |   | Int -> x > 0
               ^^^^^
Error: This expression has type bool but an expression was expected of type
         t = int
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}];;
(* Error: This expression has type bool but an expression was expected of type
t = int *)

(* OK *)
let g (type t) (x : t) (tag : t ty) : bool =
  match tag with
  | Bool -> x
  | Int -> x > 0
;;
[%%expect{|
val g : 't -> 't ty -> bool = <fun>
|}];;

let id x = x;;
let idb1 = (fun id -> let _ = id true in id) id;;
let idb2 : bool -> bool = id;;
let idb3 ( _ : bool ) = false;;

let g (type t) (x : t) (tag : t ty) =
  match tag with
  | Bool -> idb3 x
  | Int -> x > 0
;;
[%%expect{|
val id : 'a -> 'a = <fun>
val idb1 : bool -> bool = <fun>
val idb2 : bool -> bool = <fun>
val idb3 : bool -> bool = <fun>
val g : 't -> 't ty -> bool = <fun>
|}];;

let g (type t) (x : t) (tag : t ty) =
  match tag with
  | Bool -> idb2 x
  | Int -> x > 0
;;
[%%expect{|
val g : 't -> 't ty -> bool = <fun>
|}];;
