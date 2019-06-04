(* TEST
   * expect
*)

(* Type declarations using type functors. *)
type 'a t = 'a ('a. 'a -> 'a)
;;
[%%expect {|
type 'a t = 'a ('a0. 'a0 -> 'a0)
|}];;

let f (g : 'a t) (x : int) = g x
;;
[%%expect {|
val f : int t -> int -> int = <fun>
|}];;

(* Type declarations referencing variants. *)
type 'a variant = A | B of 'a
;;
[%%expect {|
type 'a variant = A | B of 'a
|}];;

type t_variant = int ('a. 'a variant)
;;
[%%expect {|
type t_variant = int ('a. 'a variant)
|}];;

let f (x : t_variant) =
  match x with
  | A -> 1
  | B x -> x
;;
[%%expect {|
val f : t_variant -> int = <fun>
|}];;

let f (x : t_variant) =
  match x with
  | A -> true
  | B x -> x
;;
[%%expect {|
Line 4, characters 11-12:
4 |   | B x -> x
               ^
Error: This expression has type int but an expression was expected of type
         bool
|}];;

(* Type functors in record fields. *)
type record = {
    a: 'a 'b. 'a ('c. 'c -> 'c) -> 'b;
    b: int ('c. 'c -> 'c)
  }
;;
[%%expect {|
type record = { a : 'a 'b. 'a ('c. 'c -> 'c) -> 'b; b : int ('c. 'c -> 'c); }
|}];;

let f (x : record) = x.a (fun (x : int) -> x)
;;
[%%expect {|
val f : record -> 'a = <fun>
|}];;

let g (x : record) : int = x.b 15
;;
[%%expect {|
val g : record -> int = <fun>
|}];;

(* Expected failure in return type. *)
let h (x : record) : bool = x.b 15
;;
[%%expect {|
Line 1, characters 28-34:
1 | let h (x : record) : bool = x.b 15
                                ^^^^^^
Error: This expression has type int but an expression was expected of type
         bool
|}];;

(* Expected failure in argument type. *)
let i (x : record) = x.b A
;;
[%%expect {|
Line 1, characters 25-26:
1 | let i (x : record) = x.b A
                             ^
Error: This expression has type 'a variant
       but an expression was expected of type int
|}];;

(* Naked type functors. *)
let x (a : int ('a. 'a)) : int = a
;;
[%%expect {|
val x : int ('a. 'a) -> int = <fun>
|}];;

let y (f : bool ('a. 'a -> 'a)) : bool = f true
;;
[%%expect {|
val y : bool ('a. 'a -> 'a) -> bool = <fun>
|}];;

let z : int ('a. 'a) = 15
;;
[%%expect {|
val z : int ('a. 'a) = 15
|}];;

let a : float ('a. 'a) variant = A
;;
[%%expect {|
val a : float ('a. 'a) variant = A
|}];;

let b : float ('a. 'a) variant = B 15.0
;;
[%%expect {|
val b : float ('a. 'a) variant = B 15.
|}];;

let c : int ('a. float) variant = B 15.0
;;
[%%expect {|
val c : int ('a. float) variant = B 15.
|}];;

(* Expected failure in GADT type. *)
let d : int ('a. int) variant = B 15.0
;;
[%%expect {|
Line 1, characters 34-38:
1 | let d : int ('a. int) variant = B 15.0
                                      ^^^^
Error: This expression has type float but an expression was expected of type
         int ('a. int) = int
|}];;
