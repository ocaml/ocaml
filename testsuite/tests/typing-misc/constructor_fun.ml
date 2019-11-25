(* TEST
   * expect
*)

let some = (Some);;
[%%expect{|
val some : 'a -> 'a option = <fun>
|}]

let _ = Some 1 = (Some) 1;;
[%%expect{|
- : bool = true
|}]

let _ = List.map (Some) [ 1 ] = [ some 1 ];;
[%%expect{|
- : bool = true
|}]

type t = Constr of int * float
let constr = (Constr);;
[%%expect{|
type t = Constr of int * float
val constr : int -> float -> t = <fun>
|}]

let _ = constr "a";;
[%%expect{|
Line 1, characters 15-18:
1 | let _ = constr "a";;
                   ^^^
Error: This expression has type string but an expression was expected of type
         int
|}]

(* Inline record *)
type t = Constr of { a : int; b : float }
let constr = (Constr);;
[%%expect{|
type t = Constr of { a : int; b : float; }
Line 2, characters 13-21:
2 | let constr = (Constr);;
                 ^^^^^^^^
Warning 67: Constructor Constr is declared using an inline record.
This will take the arguments for the fields in the order they are defined.
val constr : int -> float -> t = <fun>
|}]

let _ = constr 2 3.;;
[%%expect{|
- : t = Constr {a = 2; b = 3.}
|}]

(* Gadt *)
type _ t = Constr : int * int -> int t
let constr = (Constr);;
[%%expect{|
type _ t = Constr : int * int -> int t
val constr : int -> int -> int t = <fun>
|}]

(** Disabled *)

let some = Some;;
[%%expect{|
Line 3, characters 11-15:
3 | let some = Some;;
               ^^^^
Error: The constructor Some expects 1 argument(s),
       but is applied here to 0 argument(s)
|}]

let _ = List.map Some [ 1 ] = [ some 1 ];;
[%%expect{|
Line 1, characters 17-21:
1 | let _ = List.map Some [ 1 ] = [ some 1 ];;
                     ^^^^
Error: The constructor Some expects 1 argument(s),
       but is applied here to 0 argument(s)
|}]

let constr = Constr;;
[%%expect{|
Line 1, characters 13-19:
1 | let constr = Constr;;
                 ^^^^^^
Error: The constructor Constr expects 2 argument(s),
       but is applied here to 0 argument(s)
|}]

(* Should work with constructor disambiguation *)
type t1 = A of int
type t2 = A of int * int

let _ = (((A) 1) : t1);;
[%%expect{|
type t1 = A of int
type t2 = A of int * int
Line 4, characters 9-16:
4 | let _ = (((A) 1) : t1);;
             ^^^^^^^
Error: This expression has type int -> t2
       but an expression was expected of type t1
|}] (* TODO: Disambiguation should work, as in: *)

let _ = ((A 1) : t1);;
[%%expect{|
- : t1 = A 1
|}]

(* Type errors *)
type t = A of string
let _ = ((A) : int -> t)
[%%expect{|
type t = A of string
Line 2, characters 9-12:
2 | let _ = ((A) : int -> t)
             ^^^
Error: This expression has type int but an expression was expected of type
         string
|}] (* TODO: Error should be on the coercing and not in the generated code *)
