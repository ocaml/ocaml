(* TEST
 flags = "-dlambda -dno-locations -dcanonical-ids";
 expect;
*)

(* We explicitly enable the warning (see the discussion in the
   "Warning reference" section of the reference manual), which makes
   it clear which examples have been intentionally pessimized by the
   compiler. *)
#warnings "+degraded-to-partial-match";;
[%%expect {|
|}];;

(* The original example of unsoundness in #7421. *)
type t = {a: bool; mutable b: int option}

let f x =
  match x with
  | {a = false; b = _} -> 0
  | {a = _;     b = None} -> 1
  | {a = _;     b = _} when (x.b <- None; false) -> 2
  | {a = true;  b = Some y} -> y
;;
(* Correctness condition: there should either be a single
   (field_mut 1) access, or the second access should include
   a Match_failure case.

   PASS: the second access includes a Match_failure case. *)
[%%expect {|
0
type t = { a : bool; mutable b : int option; }
Lines 4-8, characters 2-32:
4 | ..match x with
5 |   | {a = false; b = _} -> 0
6 |   | {a = _;     b = None} -> 1
7 |   | {a = _;     b = _} when (x.b <- None; false) -> 2
8 |   | {a = true;  b = Some y} -> y
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (f/0 =
     (function x/0 : int
       (let (*match*/0 =a (field_int 0 x/0))
         (if *match*/0
           (let (*match*/1 =o (field_mut 1 x/0))
             (if *match*/1
               (if (seq (setfield_ptr 1 x/0 0) 0) 2
                 (let (*match*/2 =o (field_mut 1 x/0))
                   (if *match*/2 (let (y/0 =a (field_imm 0 *match*/2)) y/0)
                     (raise
                       (makeblock 0 (global Match_failure/0!) [0: "" 4 2])))))
               1))
           0))))
  (apply (field_mut 1 (global Toploop!)) "f" f/0))

val f : t -> int = <fun>
|}]



(* A simple example of a complete switch
   inside a mutable position. *)
type t = {a: bool; mutable b: int option}

let simple x =
  match x with
  | {b = None} -> 1
  | {b = Some y} -> y
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type t = { a : bool; mutable b : int option; }
(let
  (simple/0 =
     (function x/1 : int
       (let (*match*/3 =o (field_mut 1 x/1))
         (if *match*/3 (let (y/1 =a (field_imm 0 *match*/3)) y/1) 1))))
  (apply (field_mut 1 (global Toploop!)) "simple" simple/0))
val simple : t -> int = <fun>
|}]

(* This more complex case has the switch on [b] split across two cases
   on [a], so it may need a [Match_failure] for soundness -- it does
   if the two accesses to [b] are done on different reads of the same
   mutable field.

   PASS: two reads of [field_mut 1 x], and a Match_failure case. *)
let f x =
  match x with
  | {a = false; b = _} -> 0
  | {a = _;     b = None} -> 1
  | {a = true;  b = Some y} -> y
;;
[%%expect {|
Lines 2-5, characters 2-32:
2 | ..match x with
3 |   | {a = false; b = _} -> 0
4 |   | {a = _;     b = None} -> 1
5 |   | {a = true;  b = Some y} -> y
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (f/1 =
     (function x/2 : int
       (let (*match*/4 =a (field_int 0 x/2))
         (if *match*/4
           (let (*match*/5 =o (field_mut 1 x/2))
             (if *match*/5 (let (y/2 =a (field_imm 0 *match*/5)) y/2)
               (let (*match*/6 =o (field_mut 1 x/2))
                 (if *match*/6
                   (raise
                     (makeblock 0 (global Match_failure/0!) [0: "" 2 2]))
                   1))))
           0))))
  (apply (field_mut 1 (global Toploop!)) "f" f/1))

val f : t -> int = <fun>
|}]



(* A variant of the #7421 example. *)
let f r =
  match Some r with
  | Some { contents = None } -> 0
  | _ when (r := None; false) -> 1
  | Some { contents = Some n } -> n
  | None -> 3
;;
(* Correctness condition: there should either be a single
   (field_mut 0) access, or the second access should include
   a Match_failure case.

   PASS: two different reads (field_mut 0), and a Match_failure case. *)
[%%expect {|
Lines 2-6, characters 2-13:
2 | ..match Some r with
3 |   | Some { contents = None } -> 0
4 |   | _ when (r := None; false) -> 1
5 |   | Some { contents = Some n } -> n
6 |   | None -> 3
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (f/2 =
     (function r/0 : int
       (let (*match*/7 = (makeblock 0 r/0))
         (catch
           (if *match*/7
             (let
               (*match*/8 =a (field_imm 0 *match*/7)
                *match*/9 =o (field_mut 0 *match*/8))
               (if *match*/9 (exit 13) 0))
             (exit 13))
          with (13)
           (if (seq (setfield_ptr 0 r/0 0) 0) 1
             (if *match*/7
               (let
                 (*match*/10 =a (field_imm 0 *match*/7)
                  *match*/11 =o (field_mut 0 *match*/10))
                 (if *match*/11 (let (n/0 =a (field_imm 0 *match*/11)) n/0)
                   (raise
                     (makeblock 0 (global Match_failure/0!) [0: "" 2 2]))))
               3))))))
  (apply (field_mut 1 (global Toploop!)) "f" f/2))

val f : int option ref -> int = <fun>
|}]



(* This example has an ill-typed counter-example: the type-checker
   finds it Total, but the pattern-matching compiler cannot see that
   (Some (Some (Bool b))) cannot occur. *)
type _ t = Int : int -> int t | Bool : bool -> bool t

let test = function
  | None -> 0
  | Some (Int n) -> n
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/0 =
     (function param/0 : int
       (if param/0
         (let
           (*match*/12 =a (field_imm 0 param/0)
            n/1 =a (field_imm 0 *match*/12))
           n/1)
         0)))
  (apply (field_mut 1 (global Toploop!)) "test" test/0))
val test : int t option -> int = <fun>
|}]


(* This example has an ill-typed counter-example, inside
   a mutable position.  *)
type _ t = Int : int -> int t | Bool : bool -> bool t

let test = function
  | { contents = None } -> 0
  | { contents = Some (Int n) } -> n
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/1 =
     (function param/1 : int
       (let (*match*/13 =o (field_mut 0 param/1))
         (if *match*/13
           (let
             (*match*/14 =a (field_imm 0 *match*/13)
              n/2 =a (field_imm 0 *match*/14))
             n/2)
           0))))
  (apply (field_mut 1 (global Toploop!)) "test" test/1))
val test : int t option ref -> int = <fun>
|}]



(* This example has a ill-typed counter-example,
   and also mutable sub-patterns, but in different places. *)
type _ t = Int : int -> int t | Bool : bool -> bool t

let test n =
  match Some (ref true, Int 42) with
  | Some ({ contents = true }, Int n) -> n
  | Some ({ contents = false }, Int n) -> -n
  | None -> 3
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/2 =
     (function n/3 : int
       (let
         (*match*/15 =
            (makeblock 0 (makeblock 0 (makemutable 0 (int) 1) [0: 42])))
         (if *match*/15
           (let
             (*match*/16 =a (field_imm 0 *match*/15)
              *match*/17 =a (field_imm 0 *match*/16)
              *match*/18 =o (field_mut 0 *match*/17))
             (if *match*/18
               (let
                 (*match*/19 =a (field_imm 1 *match*/16)
                  n/4 =a (field_imm 0 *match*/19))
                 n/4)
               (let
                 (*match*/20 =a (field_imm 1 *match*/16)
                  n/5 =a (field_imm 0 *match*/20))
                 (~ n/5))))
           3))))
  (apply (field_mut 1 (global Toploop!)) "test" test/2))
val test : 'a -> int = <fun>
|}]



(* In this example, the constructor on which unsound assumptions could
   be made is not located directly below a mutable constructor, but
   one level deeper inside an immutable pair constructor (below the
   mutable constructor). This checks that there is a form of
   "transitive" propagation of mutability.

   Correctness condition: either there is a single mutable field read,
   or the accesses below the second mutable read have a Match_failure
   case.
*)
let deep r =
  match Some r with
  | Some { contents = ((), None) } -> 0
  | _ when (r := ((), None); false) -> 1
  | Some { contents = ((), Some n) } -> n
  | None -> 3
;;
(* PASS: two different reads (field_mut 0), and a Match_failure case. *)
[%%expect {|
Lines 2-6, characters 2-13:
2 | ..match Some r with
3 |   | Some { contents = ((), None) } -> 0
4 |   | _ when (r := ((), None); false) -> 1
5 |   | Some { contents = ((), Some n) } -> n
6 |   | None -> 3
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (deep/0 =
     (function r/1 : int
       (let (*match*/21 = (makeblock 0 r/1))
         (catch
           (if *match*/21
             (let
               (*match*/22 =a (field_imm 0 *match*/21)
                *match*/23 =o (field_mut 0 *match*/22)
                *match*/24 =a (field_imm 1 *match*/23))
               (if *match*/24 (exit 21) 0))
             (exit 21))
          with (21)
           (if (seq (setfield_ptr 0 r/1 [0: 0 0]) 0) 1
             (if *match*/21
               (let
                 (*match*/25 =a (field_imm 0 *match*/21)
                  *match*/26 =o (field_mut 0 *match*/25)
                  *match*/27 =a (field_imm 1 *match*/26))
                 (if *match*/27 (let (n/6 =a (field_imm 0 *match*/27)) n/6)
                   (raise
                     (makeblock 0 (global Match_failure/0!) [0: "" 2 2]))))
               3))))))
  (apply (field_mut 1 (global Toploop!)) "deep" deep/0))

val deep : (unit * int option) ref -> int = <fun>
|}]


(* In this example:
   - the pattern-matching is total, with subtle GADT usage
     (only the type-checker can tell that it is Total)
   - there are no mutable fields

   Performance expectation: there should not be a Match_failure clause.

   This example is a reduction of a regression caused by #13076 on the
   'CamlinternalFormat.trans' function in the standard library.
*)
type _ t = Bool : bool t | Int : int t | Char : char t;;
let test : type a . a t * a t -> unit = function
  | Int, Int -> ()
  | Bool, Bool -> ()
  | _, Char -> ()
;;
(* PASS: no Match_failure clause generated. *)
[%%expect {|
0
type _ t = Bool : bool t | Int : int t | Char : char t
(let
  (test/3 =
     (function param/2 : int
       (let (*match*/28 =a (field_imm 0 param/2))
         (catch
           (switch* *match*/28
            case int 0:
             (let (*match*/29 =a (field_imm 1 param/2))
               (if (>= *match*/29 2) (exit 24) 0))
            case int 1:
             (let (*match*/30 =a (field_imm 1 param/2))
               (if (>= *match*/30 2) (exit 24) 0))
            case int 2: (exit 24))
          with (24) 0))))
  (apply (field_mut 1 (global Toploop!)) "test" test/3))
val test : 'a t * 'a t -> unit = <fun>
|}];;

(* Another regression testcase from #13076, proposed by Nick Roberts.

   Performance expectation: no Match_failure clause.
*)
type nothing = |
type t = A | B | C of nothing
let f : bool * t -> int = function
  | true, A -> 3
  | false, A -> 4
  | _, B -> 5
  | _, C _ -> .
(* PASS: no Match_failure clause generated. *)
[%%expect {|
0
type nothing = |
0
type t = A | B | C of nothing
(let
  (f/3 =
     (function param/3 : int
       (let (*match*/31 =a (field_imm 0 param/3))
         (catch
           (if *match*/31
             (let (*match*/32 =a (field_imm 1 param/3))
               (switch* *match*/32 case int 0: 3
                                   case int 1: (exit 26)))
             (let (*match*/33 =a (field_imm 1 param/3))
               (switch* *match*/33 case int 0: 4
                                   case int 1: (exit 26))))
          with (26) 5))))
  (apply (field_mut 1 (global Toploop!)) "f" f/3))
val f : bool * t -> int = <fun>
|}];;


(* Another regression testcase from #13076, proposed by Nick Roberts.

   Performance expectation: no Match_failure clause.
*)
type t =
  | A of int
  | B of string
  | C of string
  | D of string

(* use primitives directly rather than Int.compare, String.compare to
   avoid offset number churns in the -dlambda test output when new
   functions get added to the stdlib. *)
external compare_int : int -> int -> int = "%compare"
external compare_string : string -> string -> int = "%compare"

let compare t1 t2 =
  match t1, t2 with
  | A i, A j -> compare_int i j
  | B l1, B l2 -> compare_string l1 l2
  | C l1, C l2 -> compare_string l1 l2
  | D l1, D l2 -> compare_string l1 l2
  | A _, (B _ | C _ | D _ ) -> -1
  | (B _ | C _ | D _ ), A _ -> 1
  | B _, (C _ | D _) -> -1
  | (C _ | D _), B _ -> 1
  | C _, D _ -> -1
  | D _, C _ -> 1
(* PASS: no Match_failure clause generated. *)
[%%expect {|
0
type t = A of int | B of string | C of string | D of string
0
external compare_int : int -> int -> int = "%compare"
0
external compare_string : string -> string -> int = "%compare"
(let
  (compare/0 =
     (function t1/0 t2/0 : int
       (catch
         (switch* t1/0
          case tag 0:
           (let (i/0 =a (field_imm 0 t1/0))
             (switch t2/0
              case tag 0:
               (let (j/0 =a (field_imm 0 t2/0)) (compare_ints i/0 j/0))
              default: -1))
          case tag 1:
           (let (l1/0 =a (field_imm 0 t1/0))
             (catch
               (switch* t2/0
                case tag 0: (exit 30)
                case tag 1:
                 (let (l2/0 =a (field_imm 0 t2/0))
                   (caml_string_compare l1/0 l2/0))
                case tag 2: (exit 35)
                case tag 3: (exit 35))
              with (35) -1))
          case tag 2:
           (let (l1/1 =a (field_imm 0 t1/0))
             (switch* t2/0
              case tag 0: (exit 30)
              case tag 1: (exit 30)
              case tag 2:
               (let (l2/1 =a (field_imm 0 t2/0))
                 (caml_string_compare l1/1 l2/1))
              case tag 3: -1))
          case tag 3:
           (let (l1/2 =a (field_imm 0 t1/0))
             (switch* t2/0
              case tag 0: (exit 30)
              case tag 1: (exit 30)
              case tag 2: 1
              case tag 3:
               (let (l2/2 =a (field_imm 0 t2/0))
                 (caml_string_compare l1/2 l2/2)))))
        with (30) (switch* t2/0 case tag 0: 1
                                case tag 1: 1))))
  (apply (field_mut 1 (global Toploop!)) "compare" compare/0))
val compare : t -> t -> int = <fun>
|}];;


(* Different testcases involving or-patterns and polymorphic variants,
   proposed by Nick Roberts. In both cases, we do *not* expect a Match_failure case. *)

let f x y =
 match x, y with
 | _, `Y1 -> 0
 | `X1, `Y2 -> 1
 | (`X2 | `X3), `Y3 -> 2
 | `X1, `Y3
 | `X2, `Y2
 | `X3, _  -> 3
(* PASS: no Match_failure generated *)
[%%expect {|
(let
  (f/4 =
     (function x/3[int] y/3[int] : int
       (catch
         (catch
           (catch (if (isint y/3) (if (!= y/3 19896) (exit 44) 0) (exit 44))
            with (44)
             (if (!= x/3 19674)
               (if (>= x/3 19675) (exit 43) (if (>= y/3 19898) (exit 41) 1))
               (if (isint y/3) (if (!= y/3 19897) (exit 43) (exit 41))
                 (exit 43))))
          with (43)
           (if (isint y/3) (if (!= y/3 19898) (exit 41) 2) (exit 41)))
        with (41) 3)))
  (apply (field_mut 1 (global Toploop!)) "f" f/4))
val f : [< `X1 | `X2 | `X3 ] -> [< `Y1 | `Y2 | `Y3 ] -> int = <fun>
|}];;


let check_results r1 r2 =
  match r1 r2 with
  | (Ok _ as r), _ | _, (Ok _ as r) -> r
  | (Error `A as r), Error _
  | Error _, (Error `A as r) -> r
  | (Error `B as r), Error `B -> r
(* PASS: no Match_failure case generated *)
[%%expect {|
(let
  (check_results/0 =
     (function r1/0 r2/0
       (let (*match*/34 = (apply r1/0 r2/0))
         (catch
           (catch
             (let (r/2 =a (field_imm 0 *match*/34))
               (catch
                 (switch* r/2
                  case tag 0: (let (r/3 =a r/2) (exit 49 r/3))
                  case tag 1:
                   (let (*match*/35 =a (field_imm 0 r/2))
                     (catch
                       (if (>= *match*/35 66)
                         (let (*match*/36 =a (field_imm 1 *match*/34))
                           (switch* *match*/36
                            case tag 0: (exit 51)
                            case tag 1:
                             (let (*match*/37 =a (field_imm 0 *match*/36))
                               (if (isint *match*/37)
                                 (if (!= *match*/37 66) (exit 52) r/2)
                                 (exit 52)))))
                         (let (*match*/38 =a (field_imm 1 *match*/34))
                           (switch* *match*/38
                            case tag 0: (exit 51)
                            case tag 1: (let (r/4 =a r/2) (exit 50 r/4)))))
                      with (52)
                       (let (r/5 =a (field_imm 1 *match*/34)) (exit 50 r/5)))))
                with (51)
                 (let (r/6 =a (field_imm 1 *match*/34)) (exit 49 r/6))))
            with (49 r/7) r/7)
          with (50 r/8) r/8))))
  (apply (field_mut 1 (global Toploop!)) "check_results" check_results/0))
val check_results :
  ('a -> ('b, [< `A | `B ]) result * ('b, [< `A | `B ]) result) ->
  'a -> ('b, [> `A | `B ]) result = <fun>
|}];;
