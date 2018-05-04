(* TEST
   flags = "-drawlambda -dno-unique-ids"
   * expect
*)

type tlist = { x: 'a. 'a list };;
[%%expect{|
0a
type tlist = { x : 'a. 'a list; }
|}];;

match { x = [] } with
| { x = [] } -> ()
| { x = 3 :: _ } -> ()
| { x = "" :: _ } -> ()
;;
[%%expect{|
(let (*match* = [0: 0a] *match* =a (field 0 *match*))
  (if *match*
    (let (*match* =a (field 0 *match*))
      (catch
        (if (!= *match* 3) (exit 1) (let (*match* =a (field 1 *match*)) 0a))
       with (1)
        (stringswitch *match*
         case "": (let (*match* =a (field 1 *match*)) 0a))))
    0a))
- : unit = ()
|}];;


type t = { x: 'a. 'a };;
[%%expect{|
0a
type t = { x : 'a. 'a; }
|}];;

match { x = assert false } with
| { x = 3 } -> ()
| { x = "" } -> ()
;;
[%%expect{|
(let
  (*match* =
     (makeblock 0
       (raise (makeblock 0 (global Assert_failureg) [0: "" 1 12])))
   *match* =a (field 0 *match*))
  (catch (if (!= *match* 3) (exit 2) 0a) with (2)
    (stringswitch *match* case "": 0a)))
Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = 3 } -> ()
| { x = None } -> ()
;;
[%%expect{|
Line _, characters 0-70:
  match { x = assert false } with
  | { x = 3 } -> ()
  | { x = None } -> ()
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{x=Some _}
(catch
  (let
    (*match* =
       (makeblock 0
         (raise (makeblock 0 (global Assert_failureg) [0: "" 1 12])))
     *match* =a (field 0 *match*))
    (catch (if (!= *match* 3) (exit 4) 0a) with (4) (if *match* (exit 3) 0a)))
 with (3) (raise (makeblock 0 (global Match_failureg) [0: "" 1 0])))
Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = None } -> ()
| { x = "" } -> ()
;;
[%%expect{|
Line _, characters 0-71:
  match { x = assert false } with
  | { x = None } -> ()
  | { x = "" } -> ()
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{x="*"}
(catch
  (let
    (*match* =
       (makeblock 0
         (raise (makeblock 0 (global Assert_failureg) [0: "" 1 12])))
     *match* =a (field 0 *match*))
    (if *match* (exit 5) 0a))
 with (5) (raise (makeblock 0 (global Match_failureg) [0: "" 1 0])))
Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = None } -> ()
| { x = `X } -> ()
;;
[%%expect{|
Line _, characters 0-71:
  match { x = assert false } with
  | { x = None } -> ()
  | { x = `X } -> ()
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{x=`AnyOtherTag}
(catch
  (let
    (*match* =
       (makeblock 0
         (raise (makeblock 0 (global Assert_failureg) [0: "" 1 12])))
     *match* =a (field 0 *match*))
    (if *match* (exit 7) 0a))
 with (7) (raise (makeblock 0 (global Match_failureg) [0: "" 1 0])))
Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = [||] } -> ()
| { x = 3 } -> ()
;;
[%%expect{|
Line _, characters 0-70:
  match { x = assert false } with
  | { x = [||] } -> ()
  | { x = 3 } -> ()
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{x=0}
(catch
  (let
    (*match* =
       (makeblock 0
         (raise (makeblock 0 (global Assert_failureg) [0: "" 1 12])))
     *match* =a (field 0 *match*))
    (catch
      (let (len =a (array.length[gen] *match*)) (if (!= len 0) (exit 10) 0a))
     with (10) (if (!= *match* 3) (exit 9) 0a)))
 with (9) (raise (makeblock 0 (global Match_failureg) [0: "" 1 0])))
Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = `X } -> ()
| { x = 3 } -> ()
;;
[%%expect{|
Line _, characters 0-68:
  match { x = assert false } with
  | { x = `X } -> ()
  | { x = 3 } -> ()
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{x=0}
(catch
  (let
    (*match* =
       (makeblock 0
         (raise (makeblock 0 (global Assert_failureg) [0: "" 1 12])))
     *match* =a (field 0 *match*))
    (catch (if (!= *match* 88) (exit 12) 0a) with (12)
      (if (!= *match* 3) (exit 11) 0a)))
 with (11) (raise (makeblock 0 (global Match_failureg) [0: "" 1 0])))
Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = `X "lol" } -> ()
| { x = 3 } -> ()
;;
[%%expect{|
Line _, characters 0-74:
  match { x = assert false } with
  | { x = `X "lol" } -> ()
  | { x = 3 } -> ()
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{x=0}
(catch
  (let
    (*match* =
       (makeblock 0
         (raise (makeblock 0 (global Assert_failureg) [0: "" 1 12])))
     *match* =a (field 0 *match*))
    (catch
      (if (isint *match*) (exit 14)
        (let (variant =a (field 0 *match*))
          (if (!= variant 88) (exit 14)
            (let (*match* =a (field 1 *match*))
              (stringswitch *match* case "lol": 0a
                                    default: (exit 13))))))
     with (14) (if (!= *match* 3) (exit 13) 0a)))
 with (13) (raise (makeblock 0 (global Match_failureg) [0: "" 1 0])))
Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = (2., "") } -> ()
| { x = None } -> ()
| { x = 3 } -> ()
;;
[%%expect{|
Line _, characters 0-95:
  match { x = assert false } with
  | { x = (2., "") } -> ()
  | { x = None } -> ()
  | { x = 3 } -> ()
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{x=0}
(catch
  (let
    (*match* =
       (makeblock 0
         (raise (makeblock 0 (global Assert_failureg) [0: "" 1 12])))
     *match* =a (field 0 *match*)
     *match* =a (field 0 *match*))
    (if (!=. *match* 2.) (exit 15)
      (let (*match* =a (field 1 *match*))
        (stringswitch *match* case "": 0a
                              default: (exit 15)))))
 with (15) (raise (makeblock 0 (global Match_failureg) [0: "" 1 0])))
Exception: Assert_failure ("", 1, 12).
|}];;
