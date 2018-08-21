(* TEST
   * expect
*)

[@@@warning "@A"];;

Pervasives.(+) 1 1;;
[%%expect{|
- : int = 2
|}]
