(* TEST
   flags = " -w A -strict-sequence "
   * expect
*)

(* Ignore OCAMLRUNPARAM=b to be reproducible *)
Printexc.record_backtrace false;;
[%%expect {|
- : unit = ()
|}]

let _ = ignore (+);;
[%%expect {|
Line 1, characters 15-18:
  let _ = ignore (+);;
                 ^^^
Warning 5: this function application is partial,
maybe some arguments are missing.
- : unit = ()
|}]

let _ = raise Exit 3;;
[%%expect {|
Line 1, characters 19-20:
  let _ = raise Exit 3;;
                     ^
Warning 20: this argument will not be used by the function.
Exception: Stdlib.Exit.
|}]
