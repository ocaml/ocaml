(* TEST
   readonly_files = "mod.ml"
   * expect
*)

#mod_use "mod.ml"
[%%expect {|
module Mod : sig val answer : int end
|}];;
