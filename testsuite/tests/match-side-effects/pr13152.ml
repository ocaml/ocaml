(* TEST
 expect;
*)

(** This example from Nick Roberts demonstrates that the following combination is possible:
    - the pattern-matching is Total according to the type-checker
    - the last clause is not taken, we get Match_failure instead
    - the scrutinee changes values, but it *never* matches the last clause.

    In particular, "optimizing" the last clause into a wildcard when
    the whole pattern-matching is total would give fairly dubious
    behavior here. We suspect that the example could be tweaked with
    judicious uses of GADTs to break type-soundness if optimized in
    this way. *)

type 'a myref = { mutable mut : 'a }
type abc = A | B | C
type t = {a: bool; b: abc myref }

let example () =
  let input = { a = true; b = { mut = A } } in
  match input with
  | {a = false; b = _} -> 1
  | {a = _;     b = { mut = B }} -> 2
  | {a = _;     b = _} when (input.b.mut <- B; false) -> 3
  | {a = true;  b = { mut = A }} -> 4
  | {a = _;     b = _} when (input.b.mut <- A; false) -> 5
  | {a = true;  b = { mut = C }} -> 6
;;

let (_ : int) = example ()
[%%expect {|
type 'a myref = { mutable mut : 'a; }
type abc = A | B | C
type t = { a : bool; b : abc myref; }
val example : unit -> int = <fun>
Exception: Match_failure ("", 18, 2).
|}];;
