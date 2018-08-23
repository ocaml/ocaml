(* TEST
   * expect
*)

[@@@warning "@A"];;

Pervasives.(+) 1 1;;
[%%expect{|
Line 3, characters 0-14:
  Pervasives.(+) 1 1;;
  ^^^^^^^^^^^^^^
Error (warning 3): deprecated: module Stdlib.Pervasives
Use Stdlib instead.
|}]

module X = Pervasives;;
[%%expect{|
Line 1, characters 11-21:
  module X = Pervasives;;
             ^^^^^^^^^^
Error (warning 3): deprecated: module Stdlib.Pervasives
Use Stdlib instead.
|}]

open Pervasives;;
[%%expect{|
Line 1, characters 5-15:
  open Pervasives;;
       ^^^^^^^^^^
Error (warning 3): deprecated: module Stdlib.Pervasives
Use Stdlib instead.
|}]
