(* TEST
   flags = " -w A -strict-sequence "
   * expect
*)

(* Ignore OCAMLRUNPARAM=b to be reproducible *)
match List.find with _ -> true;;
[%%expect {|
Line 1, characters 0-30:
1 | match List.find with _ -> true;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 68: Trying to match a function type.
- : bool = true
|}]
