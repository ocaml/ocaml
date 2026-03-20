(* TEST
 flags = " -w +A -strict-sequence ";
 expect;
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
1 | A
    ^
Warning 41 [ambiguous-name]: "A" belongs to several types: "a" "exn".
  The first one was selected. Please disambiguate if this is wrong.

- : a = A
|}]
;;

raise A
[%%expect {|
Line 1, characters 6-7:
1 | raise A
          ^
Warning 42 [disambiguated-name]: this use of "A" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

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
1 | function Not_found -> 1 | A -> 2 | _ -> 3
                              ^
Warning 42 [disambiguated-name]: this use of "A" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

- : exn -> int = <fun>
|}, Principal{|
Line 1, characters 26-27:
1 | function Not_found -> 1 | A -> 2 | _ -> 3
                              ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

Line 1, characters 26-27:
1 | function Not_found -> 1 | A -> 2 | _ -> 3
                              ^
Warning 42 [disambiguated-name]: this use of "A" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

- : exn -> int = <fun>
|}]
;;

try raise A with A -> 2
[%%expect {|
Line 1, characters 10-11:
1 | try raise A with A -> 2
              ^
Warning 42 [disambiguated-name]: this use of "A" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

Line 1, characters 17-18:
1 | try raise A with A -> 2
                     ^
Warning 42 [disambiguated-name]: this use of "A" relies on type-directed
  disambiguation, it will not compile with OCaml 4.00 or earlier.

- : int = 2
|}]
;;
