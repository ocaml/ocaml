(* TEST
   * expect
*)

module F (X : sig end) = struct type t = int end;;
type t = F(Does_not_exist).t;;
[%%expect{|
module F : functor (X : sig  end) -> sig type t = int end
Line _, characters 9-28:
  type t = F(Does_not_exist).t;;
           ^^^^^^^^^^^^^^^^^^^
Error: Unbound module Does_not_exist
|}];;
