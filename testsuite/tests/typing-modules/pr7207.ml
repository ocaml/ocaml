(* TEST
 expect;
*)

module F (X : sig end) = struct type t = int end;;
type t = F(Does_not_exist).t;;
[%%expect{|
module F : (X : sig end) -> sig type t = int end
Line 2, characters 9-28:
2 | type t = F(Does_not_exist).t;;
             ^^^^^^^^^^^^^^^^^^^
Error: Unbound module "Does_not_exist"
|}];;
