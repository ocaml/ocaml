(* TEST
   * expect
*)

type nonrec t = A : t;;
[%%expect{|
Line 1, characters 16-21:
1 | type nonrec t = A : t;;
                    ^^^^^
Error: GADT case syntax cannot be used in a 'nonrec' block.
|}]
