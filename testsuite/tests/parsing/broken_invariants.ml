(* TEST
  ppx="illegal_ppx.ml"
  * expect
*)

let empty_tuple = [%tuple];;

[%%expect{|
Line _:
Error: File "", line 6, characters 20-25:
Error: broken invariant in parsetree: Tuples must have at least 2 components.
|}]


let empty_record = [%record];;

[%%expect{|
Line _:
Error: File "", line 15, characters 21-27:
Error: broken invariant in parsetree: Records cannot be empty.
|}]

let empty_apply = [%no_args f];;

[%%expect{|
Line _:
Error: File "", line 23, characters 20-27:
Error: broken invariant in parsetree: Function application with no argument.
|}]

let f = function [%record_with_functor_fields] -> ();;
[%%expect_test {||}];;

[%%expect{|
Line _:
Error: File "", line 31, characters 19-45:
Error: broken invariant in parsetree: Functor application not allowed here.
|}]

[%%empty_let];;
[%%expect {|
Line _:
Error: File "", line 40, characters 3-12:
Error: broken invariant in parsetree: Let with no bindings.
|}]

[%%empty_type];;
[%%expect {|
Line _:
Error: File "", line 47, characters 3-13:
Error: broken invariant in parsetree: Type declarations cannot be empty.
|}]
