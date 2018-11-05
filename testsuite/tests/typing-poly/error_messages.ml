(* TEST
   * expect
*)

type t = < x : 'a. int as 'a >
[%%expect {|
Line 1, characters 15-28:
1 | type t = < x : 'a. int as 'a >
                   ^^^^^^^^^^^^^
Error: The universal type variable 'a cannot be generalized: it is bound to
       int.
|}]
type u = < x : 'a 'b. 'a as 'b >
[%%expect {|
Line 1, characters 15-30:
1 | type u = < x : 'a 'b. 'a as 'b >
                   ^^^^^^^^^^^^^^^
Error: The universal type variable 'b cannot be generalized:
       it is already bound to another variable.
|}]
type v = 'b -> < x : 'a. 'b as 'a >
[%%expect {|
Line 1, characters 21-33:
1 | type v = 'b -> < x : 'a. 'b as 'a >
                         ^^^^^^^^^^^^
Error: The universal type variable 'a cannot be generalized:
       it escapes its scope.
|}]
