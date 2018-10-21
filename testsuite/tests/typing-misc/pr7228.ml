(* TEST
   * expect
*)

type t = A of {mutable x: int};;
fun (A r) -> r.x <- 42;;
[%%expect{|
type t = A of { mutable x : int; }
- : t -> unit = <fun>
|}];;

(* Check that mutability is blocked for inline records on private types *)
type t = private A of {mutable x: int};;
fun (A r) -> r.x <- 42;;
[%%expect{|
type t = private A of { mutable x : int; }
Line 2, characters 15-16:
2 | fun (A r) -> r.x <- 42;;
                   ^
Error: Cannot assign field x of the private type t.A
|}];;
