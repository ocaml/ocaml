(* TEST
   * expect
*)

module Test (S : sig module type S end) (M : S.S) =
  struct open M (* should not succeed silently *) end
[%%expect{|
Line 2, characters 14-15:
2 |   struct open M (* should not succeed silently *) end
                  ^
Error: This module is not a structure; it has type S.S
|}]
