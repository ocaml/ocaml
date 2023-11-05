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

   FAIL: the second occurrence of (field_mut 1) is used with a direct
   (field_imm 0) access without a constructor check. The compiler is
   unsound here. *)
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
                 (field_imm 0 *match*/288)))
             1))
         0)))
  (apply (field_mut 1 (global Toploop!)) "f" f/281))
val f : t -> int = <fun>
|}]



(* A simple example of a complete switch
   inside a mutable position. *)
type t = {a: bool; mutable b: int option}

let f x =
  match x with
  | {a = false; b = _} -> 0
  | {a = _;     b = None} -> 1
  | {a = true;  b = Some y} -> y
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type t = { a : bool; mutable b : int option; }
(let
  (f/292 =
     (function x/293 : int
       (if (field_int 0 x/293)
         (let (*match*/297 =o (field_mut 1 x/293))
           (if *match*/297 (field_imm 0 *match*/297) 1))
         0)))
  (apply (field_mut 1 (global Toploop!)) "f" f/292))
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
   (field_mut 1) access, or the second access should include
   a Match_failure case.

   FAIL: the second occurrence of (field_mut 0) is used with a direct
   (field_imm 0) access without a constructor check. The compiler is
   unsound here. *)
[%%expect {|
(let
  (f/299 =
     (function r/300 : int
       (let (*match*/302 = (makeblock 0 r/300))
         (catch
           (if *match*/302
             (let (*match*/304 =o (field_mut 0 (field_imm 0 *match*/302)))
               (if *match*/304 (exit 7) 0))
             (exit 7))
          with (7)
           (if (seq (setfield_ptr 0 r/300 0) 0) 1
             (if *match*/302
               (let (*match*/306 =o (field_mut 0 (field_imm 0 *match*/302)))
                 (field_imm 0 *match*/306))
               3))))))
  (apply (field_mut 1 (global Toploop!)) "f" f/299))
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
  (test/310 =
     (function param/313 : int
       (if param/313 (field_imm 0 (field_imm 0 param/313)) 0)))
  (apply (field_mut 1 (global Toploop!)) "test" test/310))
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
  (test/318 =
     (function param/320 : int
       (let (*match*/321 =o (field_mut 0 param/320))
         (if *match*/321 (field_imm 0 (field_imm 0 *match*/321)) 0))))
  (apply (field_mut 1 (global Toploop!)) "test" test/318))
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
  (test/326 =
     (function n/327 : int
       (let
         (*match*/330 =
            (makeblock 0 (makeblock 0 (makemutable 0 (int) 1) [0: 42])))
         (if *match*/330
           (let
             (*match*/331 =a (field_imm 0 *match*/330)
              *match*/333 =o (field_mut 0 (field_imm 0 *match*/331)))
             (if *match*/333 (field_imm 0 (field_imm 1 *match*/331))
               (~ (field_imm 0 (field_imm 1 *match*/331)))))
           3))))
  (apply (field_mut 1 (global Toploop!)) "test" test/326))
val test : 'a -> int = <fun>
|}]
