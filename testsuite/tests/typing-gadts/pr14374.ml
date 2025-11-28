(* TEST
 expect;
*)

type _ ty =
| All : 'a ty
| AandB : [< `A | `B] ty

let test (c : [< `C ] ty) =
  match c with
  | All -> 1
[%%expect{|
type _ ty = All : 'a ty | AandB : [< `A | `B ] ty
val test : [< `C ] ty -> int = <fun>
|}]

(* error if empty polymorphic variants are not allowed *)
let _ = test AandB
[%%expect{|
Line 1, characters 13-18:
1 | let _ = test AandB
                 ^^^^^
Error: The constructor "AandB" has type "[< `A | `B ] ty"
       but an expression was expected of type "[< `C ] ty"
       These two variant types have no intersection
|}]
