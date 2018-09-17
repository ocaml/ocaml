(* TEST
   * expect
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
Line 1, characters 0-70:
  match { x = assert false } with
  | { x = 3 } -> ()
  | { x = None } -> ()
Warning 8:
this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{x=Some _}
Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = None } -> ()
| { x = "" } -> ()
;;
[%%expect{|
Line 1, characters 0-71:
  match { x = assert false } with
  | { x = None } -> ()
  | { x = "" } -> ()
Warning 8:
this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{x="*"}
Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = None } -> ()
| { x = `X } -> ()
;;
[%%expect{|
Line 1, characters 0-71:
  match { x = assert false } with
  | { x = None } -> ()
  | { x = `X } -> ()
Warning 8:
this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{x=`AnyOtherTag}
Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = [||] } -> ()
| { x = 3 } -> ()
;;
[%%expect{|
Line 1, characters 0-70:
  match { x = assert false } with
  | { x = [||] } -> ()
  | { x = 3 } -> ()
Warning 8:
this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{x=0}
Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = `X } -> ()
| { x = 3 } -> ()
;;
[%%expect{|
Line 1, characters 0-68:
  match { x = assert false } with
  | { x = `X } -> ()
  | { x = 3 } -> ()
Warning 8:
this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{x=0}
Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = `X "lol" } -> ()
| { x = 3 } -> ()
;;
[%%expect{|
Line 1, characters 0-74:
  match { x = assert false } with
  | { x = `X "lol" } -> ()
  | { x = 3 } -> ()
Warning 8:
this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{x=0}
Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = (2., "") } -> ()
| { x = None } -> ()
| { x = 3 } -> ()
;;
[%%expect{|
Line 1, characters 0-95:
  match { x = assert false } with
  | { x = (2., "") } -> ()
  | { x = None } -> ()
  | { x = 3 } -> ()
Warning 8:
this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{x=0}
Exception: Assert_failure ("", 1, 12).
|}];;
