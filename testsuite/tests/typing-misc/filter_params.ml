(* TEST
   * expect
*)

type ('a, 'b) t constraint 'a = 'b
[%%expect{|
type ('b, 'a) t constraint 'a = 'b
|}]
