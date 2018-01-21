(* TEST
   * expect
*)

(* PR#7324 *)

module rec T : sig type t = T.t end = T;;
[%%expect{|
Line _, characters 15-35:
  module rec T : sig type t = T.t end = T;;
                 ^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation T.t is cyclic
|}]
