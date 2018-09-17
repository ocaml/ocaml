(* TEST
   flags = " -w A -strict-sequence "
   * expect
*)

(* Ignore OCAMLRUNPARAM=b to be reproducible *)
Printexc.record_backtrace false
[%%expect {|
- : unit = ()
|}]
;;

exception A
[%%expect {|
exception A
|}]
;;

type a = A
[%%expect {|
type a = A
|}]
;;

A
[%%expect {|
Line 1, characters 0-1:
  A
  ^
Warning 41:
A belongs to several types: a exn
The first one was selected. Please disambiguate if this is wrong.
- : a = A
|}]
;;

raise A
[%%expect {|
Line 1, characters 6-7:
  raise A
        ^
Warning 42:
this use of A relies on type-directed disambiguation,
it will not compile with OCaml 4.00 or earlier.
Exception: A.
|}]
;;

fun (A : a) -> ()
[%%expect {|
- : a -> unit = <fun>
|}]
;;

function Not_found -> 1 | A -> 2 | _ -> 3
[%%expect {|
Line 1, characters 26-27:
  function Not_found -> 1 | A -> 2 | _ -> 3
                            ^
Warning 42:
this use of A relies on type-directed disambiguation,
it will not compile with OCaml 4.00 or earlier.
- : exn -> int = <fun>
|}, Principal{|
Line 1, characters 26-27:
  function Not_found -> 1 | A -> 2 | _ -> 3
                            ^
Warning 41:
A belongs to several types: a exn
The first one was selected. Please disambiguate if this is wrong.
Line 1, characters 26-27:
  function Not_found -> 1 | A -> 2 | _ -> 3
                            ^
Error:
This pattern matches values of type a
but a pattern was expected which matches values of type exn
|}]
;;

try raise A with A -> 2
[%%expect {|
Line 1, characters 10-11:
  try raise A with A -> 2
            ^
Warning 42:
this use of A relies on type-directed disambiguation,
it will not compile with OCaml 4.00 or earlier.
Line 1, characters 17-18:
  try raise A with A -> 2
                   ^
Warning 42:
this use of A relies on type-directed disambiguation,
it will not compile with OCaml 4.00 or earlier.
- : int = 2
|}]
;;
