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
