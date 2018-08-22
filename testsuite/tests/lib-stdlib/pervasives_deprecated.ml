(* TEST
   * expect
*)

[@@@warning "@A"];;

Pervasives.(+) 1 1;;
[%%expect{|
- : int = 2
|}]

module X = Pervasives;;
[%%expect{|
module X = Pervasives
|}]

open Pervasives;;
[%%expect{|
|}]
