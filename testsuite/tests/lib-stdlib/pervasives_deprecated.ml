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

If you need to stay compatible with OCaml < 4.07, you can use the
stdlib-shims library: https://github.com/ocaml/stdlib-shims
|}]

module X = Pervasives;;
[%%expect{|
Line 1, characters 11-21:
  module X = Pervasives;;
             ^^^^^^^^^^
Error (warning 3): deprecated: module Stdlib.Pervasives
Use Stdlib instead.

If you need to stay compatible with OCaml < 4.07, you can use the
stdlib-shims library: https://github.com/ocaml/stdlib-shims
|}]

open Pervasives;;
[%%expect{|
Line 1, characters 5-15:
  open Pervasives;;
       ^^^^^^^^^^
Error (warning 3): deprecated: module Stdlib.Pervasives
Use Stdlib instead.

If you need to stay compatible with OCaml < 4.07, you can use the
stdlib-shims library: https://github.com/ocaml/stdlib-shims
|}]
