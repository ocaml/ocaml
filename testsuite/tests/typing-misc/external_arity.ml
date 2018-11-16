(* TEST
   * expect
*)

let f a b c = a + b + c

let _ : int = Obj.magic f None None None

[%%expect
{|
val f : int -> int -> int -> int = <fun>
- : int = 0
|}]

external cmp : 'a -> 'b = "%compare"

[%%expect
{|
Line 1, characters 0-36:
1 | external cmp : 'a -> 'b = "%compare"
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Wrong arity for builtin primitive "%compare"
|}]

external apply : 'a -> 'b = "%apply"

[%%expect
{|
Line 1, characters 0-36:
1 | external apply : 'a -> 'b = "%apply"
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Wrong arity for builtin primitive "%apply"
|}]
