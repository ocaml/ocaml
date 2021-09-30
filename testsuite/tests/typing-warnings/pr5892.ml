(* TEST
   flags = " -w +A -strict-sequence "
   * expect
*)

open CamlinternalOO;;

type _ choice = Left : label choice | Right : tag choice;;
[%%expect {|
type _ choice =
    Left : CamlinternalOO.label choice
  | Right : CamlinternalOO.tag choice
|}]

let f : label choice -> bool = function Left -> true;; (* warn *)
[%%expect {|
Line 1, characters 31-52:
1 | let f : label choice -> bool = function Left -> true;; (* warn *)
                                   ^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Right
val f : CamlinternalOO.label choice -> bool = <fun>
|}]
