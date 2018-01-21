(* TEST
   * expect
*)

type nonrec t = A : t;;
[%%expect{|
Line _, characters 16-21:
  type nonrec t = A : t;;
                  ^^^^^
Error: GADT case syntax cannot be used in a 'nonrec' block.
|}]

