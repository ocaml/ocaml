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
