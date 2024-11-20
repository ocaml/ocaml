(* TEST
 flags = "-dlambda";
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
  (f/281 =
     (function x/283 : int
       (if (field_int 0 x/283)
         (let (*match*/287 =o (field_mut 1 x/283))
           (if *match*/287
             (if (seq (setfield_ptr 1 x/283 0) 0) 2
               (let (*match*/288 =o (field_mut 1 x/283))
                 (if *match*/288 (field_imm 0 *match*/288)
                   (raise
                     (makeblock 0 (global Match_failure/20!) [0: "" 4 2])))))
             1))
         0)))
  (apply (field_mut 1 (global Toploop!)) "f" f/281))

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
  (simple/292 =
     (function x/294 : int
       (let (*match*/297 =o (field_mut 1 x/294))
         (if *match*/297 (field_imm 0 *match*/297) 1))))
  (apply (field_mut 1 (global Toploop!)) "simple" simple/292))
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
  (f/298 =
     (function x/299 : int
       (if (field_int 0 x/299)
         (let (*match*/303 =o (field_mut 1 x/299))
           (if *match*/303 (field_imm 0 *match*/303)
             (let (*match*/304 =o (field_mut 1 x/299))
               (if *match*/304
                 (raise (makeblock 0 (global Match_failure/20!) [0: "" 2 2]))
                 1))))
         0)))
  (apply (field_mut 1 (global Toploop!)) "f" f/298))

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
  (f/305 =
     (function r/306 : int
       (let (*match*/308 = (makeblock 0 r/306))
         (catch
           (if *match*/308
             (let (*match*/310 =o (field_mut 0 (field_imm 0 *match*/308)))
               (if *match*/310 (exit 13) 0))
             (exit 13))
          with (13)
           (if (seq (setfield_ptr 0 r/306 0) 0) 1
             (if *match*/308
               (let (*match*/312 =o (field_mut 0 (field_imm 0 *match*/308)))
                 (if *match*/312 (field_imm 0 *match*/312)
                   (raise
                     (makeblock 0 (global Match_failure/20!) [0: "" 2 2]))))
               3))))))
  (apply (field_mut 1 (global Toploop!)) "f" f/305))

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
  (test/316 =
     (function param/319 : int
       (if param/319 (field_imm 0 (field_imm 0 param/319)) 0)))
  (apply (field_mut 1 (global Toploop!)) "test" test/316))
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
  (test/324 =
     (function param/326 : int
       (let (*match*/327 =o (field_mut 0 param/326))
         (if *match*/327 (field_imm 0 (field_imm 0 *match*/327)) 0))))
  (apply (field_mut 1 (global Toploop!)) "test" test/324))
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
  (test/332 =
     (function n/333 : int
       (let
         (*match*/336 =
            (makeblock 0 (makeblock 0 (makemutable 0 (int) 1) [0: 42])))
         (if *match*/336
           (let
             (*match*/337 =a (field_imm 0 *match*/336)
              *match*/339 =o (field_mut 0 (field_imm 0 *match*/337)))
             (if *match*/339 (field_imm 0 (field_imm 1 *match*/337))
               (~ (field_imm 0 (field_imm 1 *match*/337)))))
           3))))
  (apply (field_mut 1 (global Toploop!)) "test" test/332))
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
  (deep/342 =
     (function r/344 : int
       (let (*match*/346 = (makeblock 0 r/344))
         (catch
           (if *match*/346
             (let (*match*/348 =o (field_mut 0 (field_imm 0 *match*/346)))
               (if (field_imm 1 *match*/348) (exit 21) 0))
             (exit 21))
          with (21)
           (if (seq (setfield_ptr 0 r/344 [0: 0 0]) 0) 1
             (if *match*/346
               (let
                 (*match*/352 =o (field_mut 0 (field_imm 0 *match*/346))
                  *match*/354 =a (field_imm 1 *match*/352))
                 (if *match*/354 (field_imm 0 *match*/354)
                   (raise
                     (makeblock 0 (global Match_failure/20!) [0: "" 2 2]))))
               3))))))
  (apply (field_mut 1 (global Toploop!)) "deep" deep/342))

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
  (test/359 =
     (function param/361 : int
       (catch
         (if (>= (field_imm 0 param/361) 2) (exit 24)
           (if (>= (field_imm 1 param/361) 2) (exit 24) 0))
        with (24) 0)))
  (apply (field_mut 1 (global Toploop!)) "test" test/359))
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
  (f/371 =
     (function param/372 : int
       (catch
         (if (field_imm 0 param/372)
           (switch* (field_imm 1 param/372)
            case int 0: 3
            case int 1: (exit 27))
           (switch* (field_imm 1 param/372)
            case int 0: 4
            case int 1: (exit 27)))
        with (27) 5)))
  (apply (field_mut 1 (global Toploop!)) "f" f/371))
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

let compare t1 t2 =
  match t1, t2 with
  | A i, A j -> Int.compare i j
  | B l1, B l2 -> String.compare l1 l2
  | C l1, C l2 -> String.compare l1 l2
  | D l1, D l2 -> String.compare l1 l2
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
(let
  (compare/382 =
     (function t1/383 t2/384 : int
       (catch
         (switch* t1/383
          case tag 0:
           (switch t2/384
            case tag 0:
             (apply (field_imm 8 (global Stdlib__Int!)) (field_imm 0 t1/383)
               (field_imm 0 t2/384))
            default: -1)
          case tag 1:
           (catch
             (switch* t2/384
              case tag 0: (exit 31)
              case tag 1:
               (apply (field_imm 9 (global Stdlib__String!))
                 (field_imm 0 t1/383) (field_imm 0 t2/384))
              case tag 2: (exit 36)
              case tag 3: (exit 36))
            with (36) -1)
          case tag 2:
           (switch* t2/384
            case tag 0: (exit 31)
            case tag 1: (exit 31)
            case tag 2:
             (apply (field_imm 9 (global Stdlib__String!))
               (field_imm 0 t1/383) (field_imm 0 t2/384))
            case tag 3: -1)
          case tag 3:
           (switch* t2/384
            case tag 0: (exit 31)
            case tag 1: (exit 31)
            case tag 2: 1
            case tag 3:
             (apply (field_imm 9 (global Stdlib__String!))
               (field_imm 0 t1/383) (field_imm 0 t2/384))))
        with (31) (switch* t2/384 case tag 0: 1
                                  case tag 1: 1))))
  (apply (field_mut 1 (global Toploop!)) "compare" compare/382))
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
  (f/504 =
     (function x/505[int] y/506[int] : int
       (catch
         (catch
           (catch
             (if (isint y/506) (if (!= y/506 19896) (exit 45) 0) (exit 45))
            with (45)
             (if (!= x/505 19674)
               (if (>= x/505 19675) (exit 44)
                 (if (>= y/506 19898) (exit 42) 1))
               (if (isint y/506) (if (!= y/506 19897) (exit 44) (exit 42))
                 (exit 44))))
          with (44)
           (if (isint y/506) (if (!= y/506 19898) (exit 42) 2) (exit 42)))
        with (42) 3)))
  (apply (field_mut 1 (global Toploop!)) "f" f/504))
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
  (check_results/507 =
     (function r1/509 r2/510
       (let (*match*/516 = (apply r1/509 r2/510))
         (catch
           (catch
             (let (r/515 =a (field_imm 0 *match*/516))
               (catch
                 (switch* r/515
                  case tag 0: (exit 50 r/515)
                  case tag 1:
                   (catch
                     (if (>= (field_imm 0 r/515) 66)
                       (let (*match*/524 =a (field_imm 1 *match*/516))
                         (switch* *match*/524
                          case tag 0: (exit 52)
                          case tag 1:
                           (let (*match*/525 =a (field_imm 0 *match*/524))
                             (if (isint *match*/525)
                               (if (!= *match*/525 66) (exit 53) r/515)
                               (exit 53)))))
                       (switch* (field_imm 1 *match*/516)
                        case tag 0: (exit 52)
                        case tag 1: (exit 51 r/515)))
                    with (53) (exit 51 (field_imm 1 *match*/516))))
                with (52) (exit 50 (field_imm 1 *match*/516))))
            with (50 r/511) r/511)
          with (51 r/513) r/513))))
  (apply (field_mut 1 (global Toploop!)) "check_results" check_results/507))
val check_results :
  ('a -> ('b, [< `A | `B ]) result * ('b, [< `A | `B ]) result) ->
  'a -> ('b, [> `A | `B ]) result = <fun>
|}];;
