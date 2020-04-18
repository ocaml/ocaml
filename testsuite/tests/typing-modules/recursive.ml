(* TEST
   * expect
*)

(* PR#7324 *)

module rec T : sig type t = T.t end = T;;
[%%expect{|
Line 1, characters 0-39:
1 | module rec T : sig type t = T.t end = T;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation T.t is cyclic
|}]
