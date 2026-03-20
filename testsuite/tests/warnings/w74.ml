(* TEST
   flags = "-w +74";
   expect;
*)

#debug true;;
(* Being in debug mode is important here, because otherwise OCaml
   compilers the pattern-matching differently (it shares more actions)
   and does not introduce a Match_failure case in some examples
   ('f' below), so those don't warn anymore. *)

(* Warning expected. *)
let f : bool * bool ref -> unit = function
| (true, {contents = true}) -> ()
| (_, r) when (r.contents <- true; false) -> assert false
| (false, _) -> ()
| (_, {contents = false}) -> ()
[%%expect {|
Lines 1-5, characters 34-31:
1 | ..................................function
2 | | (true, {contents = true}) -> ()
3 | | (_, r) when (r.contents <- true; false) -> assert false
4 | | (false, _) -> ()
5 | | (_, {contents = false}) -> ()
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)

val f : bool * bool ref -> unit = <fun>
|}];;

(* warning expected (concurrent mutations) *)
let g : bool * bool ref -> unit = function
| (true, {contents = true}) -> ()
| (false, _) -> ()
| (_, {contents = false}) -> ()
[%%expect {|
Lines 1-4, characters 34-31:
1 | ..................................function
2 | | (true, {contents = true}) -> ()
3 | | (false, _) -> ()
4 | | (_, {contents = false}) -> ()
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)

val g : bool * bool ref -> unit = <fun>
|}];;

(* no warning expected (single read of the 'contents' field) *)
let h : bool ref -> unit = function
| {contents = true} -> ()
| r when (r.contents <- true; false) -> assert false
| {contents = false} -> ()
[%%expect {|
val h : bool ref -> unit = <fun>
|}];;

(* Check that the warning can be disabled.

   FAIL: currently one cannot locally disable the warnings emitted
   during Lambda production, only the type-checking warnings.
*)
let f : bool * bool ref -> unit = fun p ->
  match[@warning "-74"] p with
  | (true, {contents = true}) -> ()
  | (_, r) when (r.contents <- true; false) -> assert false
  | (false, _) -> ()
  | (_, {contents = false}) -> ()
[%%expect {|
Lines 2-6, characters 2-33:
2 | ..match[@warning "-74"] p with
3 |   | (true, {contents = true}) -> ()
4 |   | (_, r) when (r.contents <- true; false) -> assert false
5 |   | (false, _) -> ()
6 |   | (_, {contents = false}) -> ()
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)

val f : bool * bool ref -> unit = <fun>
|}];;

let f : bool * bool ref -> unit = function[@warning "-74"]
| (true, {contents = true}) -> ()
| (_, r) when (r.contents <- true; false) -> assert false
| (false, _) -> ()
| (_, {contents = false}) -> ()
[%%expect {|
Lines 1-5, characters 34-31:
1 | ..................................function[@warning "-74"]
2 | | (true, {contents = true}) -> ()
3 | | (_, r) when (r.contents <- true; false) -> assert false
4 | | (false, _) -> ()
5 | | (_, {contents = false}) -> ()
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)

val f : bool * bool ref -> unit = <fun>
|}];;
