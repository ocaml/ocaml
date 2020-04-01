(* TEST
   * expect
*)

(* Example from Stephen Dolan.
   Accessing an extension constructor involves accessing the module
   in which it's defined.
 *)
module type T =
  sig exception A of int end;;
[%%expect{|
module type T = sig exception A of int end
|}];;

let rec x =
  let module M = (val m) in
  M.A 42
and (m : (module T)) =
  (module (struct exception A of int end) : T);;
[%%expect{|
Lines 2-3, characters 2-8:
2 | ..let module M = (val m) in
3 |   M.A 42
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;
