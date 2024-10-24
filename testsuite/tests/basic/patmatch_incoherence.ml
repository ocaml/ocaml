(* TEST
 expect;
*)

type tlist = { x: 'a. 'a list };;
[%%expect{|
type tlist = { x : 'a. 'a list; }
|}];;

match { x = [] } with
| { x = [] } -> ()
| { x = 3 :: _ } -> ()
| { x = "" :: _ } -> ()
;;
[%%expect{|
- : unit = ()
|}];;


type t = { x: 'a. 'a };;
[%%expect{|
type t = { x : 'a. 'a; }
|}];;

match { x = assert false } with
| { x = 3 } -> ()
| { x = "" } -> ()
;;
[%%expect{|
Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = 3 } -> ()
| { x = None } -> ()
;;
[%%expect{|
Lines 1-3, characters 0-20:
1 | match { x = assert false } with
2 | | { x = 3 } -> ()
3 | | { x = None } -> ()
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "{x=Some _}"

Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = None } -> ()
| { x = "" } -> ()
;;
[%%expect{|
Lines 1-3, characters 0-18:
1 | match { x = assert false } with
2 | | { x = None } -> ()
3 | | { x = "" } -> ()
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "{x="*"}"

Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = None } -> ()
| { x = `X } -> ()
;;
[%%expect{|
Lines 1-3, characters 0-18:
1 | match { x = assert false } with
2 | | { x = None } -> ()
3 | | { x = `X } -> ()
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "{x=`AnyOtherTag}"

Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = [||] } -> ()
| { x = 3 } -> ()
;;
[%%expect{|
Lines 1-3, characters 0-17:
1 | match { x = assert false } with
2 | | { x = [||] } -> ()
3 | | { x = 3 } -> ()
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "{x=0}"

Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = `X } -> ()
| { x = 3 } -> ()
;;
[%%expect{|
Lines 1-3, characters 0-17:
1 | match { x = assert false } with
2 | | { x = `X } -> ()
3 | | { x = 3 } -> ()
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "{x=0}"

Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = `X "lol" } -> ()
| { x = 3 } -> ()
;;
[%%expect{|
Lines 1-3, characters 0-17:
1 | match { x = assert false } with
2 | | { x = `X "lol" } -> ()
3 | | { x = 3 } -> ()
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "{x=0}"

Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = (2., "") } -> ()
| { x = None } -> ()
| { x = 3 } -> ()
;;
[%%expect{|
Lines 1-4, characters 0-17:
1 | match { x = assert false } with
2 | | { x = (2., "") } -> ()
3 | | { x = None } -> ()
4 | | { x = 3 } -> ()
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "{x=0}"

Exception: Assert_failure ("", 1, 12).
|}];;
