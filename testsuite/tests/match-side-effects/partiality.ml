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
  (f/279 =
     (function x/281 : int
       (let (*strict*/283 =o (field_mut 1 x/281))
         (if (field_int 0 x/281)
           (if *strict*/283
             (if (seq (setfield_ptr 1 x/281 0) 0) 2
               (field_imm 0 *strict*/283))
             1)
           0))))
  (apply (field_mut 1 (global Toploop!)) "f" f/279))
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
  (f/288 =
     (function x/289 : int
       (let (*strict*/291 =o (field_mut 1 x/289))
         (if (field_int 0 x/289)
           (if *strict*/291 (field_imm 0 *strict*/291) 1) 0))))
  (apply (field_mut 1 (global Toploop!)) "f" f/288))
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
  (f/293 =
     (function r/294 : int
       (let (*match*/296 = (makeblock 0 r/294))
         (catch
           (if *match*/296
             (let (*strict*/298 =o (field_mut 0 (field_imm 0 *match*/296)))
               (if *strict*/298 (exit 7) 0))
             (exit 7))
          with (7)
           (if (seq (setfield_ptr 0 r/294 0) 0) 1
             (if *match*/296
               (let (*strict*/300 =o (field_mut 0 (field_imm 0 *match*/296)))
                 (field_imm 0 *strict*/300))
               3))))))
  (apply (field_mut 1 (global Toploop!)) "f" f/293))
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
  (test/304 =
     (function param/307 : int
       (if param/307 (field_imm 0 (field_imm 0 param/307)) 0)))
  (apply (field_mut 1 (global Toploop!)) "test" test/304))
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
  (test/312 =
     (function param/314 : int
       (let (*strict*/315 =o (field_mut 0 param/314))
         (if *strict*/315 (field_imm 0 (field_imm 0 *strict*/315)) 0))))
  (apply (field_mut 1 (global Toploop!)) "test" test/312))
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
  (test/320 =
     (function n/321 : int
       (let
         (*match*/324 =
            (makeblock 0 (makeblock 0 (makemutable 0 (int) 1) [0: 42])))
         (if *match*/324
           (let
             (*match*/325 =a (field_imm 0 *match*/324)
              *strict*/327 =o (field_mut 0 (field_imm 0 *match*/325)))
             (if *strict*/327 (field_imm 0 (field_imm 1 *match*/325))
               (~ (field_imm 0 (field_imm 1 *match*/325)))))
           3))))
  (apply (field_mut 1 (global Toploop!)) "test" test/320))
val test : 'a -> int = <fun>
|}]
