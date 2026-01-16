(* TEST
  expect;
*)

(* Type constraints are inconsistent because of double vision *)

module rec X : sig
  type 'a t = 'a X.s as 'b
    constraint 'b = int
  type 'a s = int
end = X
[%%expect {|
Line 3, characters 15-23:
3 |     constraint 'b = int
                   ^^^^^^^^
Error: The type constraints are not consistent.
       Type "'a X.s" is not compatible with type "int"
       Type "X.s" was considered abstract when checking constraints in this
       recursive module definition.
|}]
