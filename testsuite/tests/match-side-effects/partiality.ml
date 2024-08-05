(* TEST
 flags = "-dlambda";
 expect;
*)

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

   PASS: a single read of [field_mut 1 x], no Match_failure case. *)
let f x =
  match x with
  | {a = false; b = _} -> 0
  | {a = _;     b = None} -> 1
  | {a = true;  b = Some y} -> y
;;
[%%expect {|
(let
  (f/298 =
     (function x/299 : int
       (if (field_int 0 x/299)
         (let (*match*/303 =o (field_mut 1 x/299))
           (if *match*/303 (field_imm 0 *match*/303) 1))
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

   FAIL: the second occurrence of (field_mut 0) is used with a direct
   (field_imm 0) access without a constructor check. The compiler is
   unsound here. *)
[%%expect {|
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
                 (field_imm 0 *match*/312))
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
(* FAIL: two different reads (field_mut 0), but no Match_failure case. *)
[%%expect {|
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
               (let (*match*/352 =o (field_mut 0 (field_imm 0 *match*/346)))
                 (field_imm 0 (field_imm 1 *match*/352)))
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
(* FAIL: currently a Match_failure clause is generated. *)
[%%expect {|
0
type _ t = Bool : bool t | Int : int t | Char : char t
(let
  (test/359 =
     (function param/361 : int
       (catch
         (catch
           (switch* (field_imm 0 param/361)
            case int 0:
             (switch* (field_imm 1 param/361)
              case int 0: 0
              case int 1: (exit 23)
              case int 2: (exit 24))
            case int 1:
             (switch* (field_imm 1 param/361)
              case int 0: (exit 23)
              case int 1: 0
              case int 2: (exit 24))
            case int 2: (exit 24))
          with (24) 0)
        with (23)
         (raise (makeblock 0 (global Match_failure/20!) [0: "" 2 40])))))
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
(* FAIL: a Match_failure clause is generated. *)
[%%expect {|
0
type nothing = |
0
type t = A | B | C of nothing
(let
  (f/371 =
     (function param/372 : int
       (catch
         (catch
           (if (field_imm 0 param/372)
             (let (*match*/374 =a (field_imm 1 param/372))
               (if (isint *match*/374) (if *match*/374 (exit 26) 3)
                 (exit 25)))
             (let (*match*/375 =a (field_imm 1 param/372))
               (if (isint *match*/375) (if *match*/375 (exit 26) 4)
                 (exit 25))))
          with (26) 5)
        with (25)
         (raise (makeblock 0 (global Match_failure/20!) [0: "" 3 26])))))
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
(* FAIL: a Match_failure clause is generated. *)
[%%expect {|
0
type t = A of int | B of string | C of string | D of string
(let
  (compare/382 =
     (function t1/383 t2/384 : int
       (catch
         (catch
           (switch* t1/383
            case tag 0:
             (switch t2/384
              case tag 0:
               (apply (field_imm 8 (global Stdlib__Int!))
                 (field_imm 0 t1/383) (field_imm 0 t2/384))
              default: -1)
            case tag 1:
             (catch
               (switch* t2/384
                case tag 0: (exit 30)
                case tag 1:
                 (apply (field_imm 9 (global Stdlib__String!))
                   (field_imm 0 t1/383) (field_imm 0 t2/384))
                case tag 2: (exit 35)
                case tag 3: (exit 35))
              with (35) -1)
            case tag 2:
             (switch* t2/384
              case tag 0: (exit 30)
              case tag 1: (exit 30)
              case tag 2:
               (apply (field_imm 9 (global Stdlib__String!))
                 (field_imm 0 t1/383) (field_imm 0 t2/384))
              case tag 3: -1)
            case tag 3:
             (switch* t2/384
              case tag 0: (exit 30)
              case tag 1: (exit 30)
              case tag 2: 1
              case tag 3:
               (apply (field_imm 9 (global Stdlib__String!))
                 (field_imm 0 t1/383) (field_imm 0 t2/384))))
          with (30)
           (switch* t2/384
            case tag 0: 1
            case tag 1: 1
            case tag 2: (exit 27)
            case tag 3: (exit 27)))
        with (27)
         (raise (makeblock 0 (global Match_failure/20!) [0: "" 8 2])))))
  (apply (field_mut 1 (global Toploop!)) "compare" compare/382))
val compare : t -> t -> int = <fun>
|}];;
