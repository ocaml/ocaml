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
       (if (field_int 0 x/281)
         (let (*match*/285 =o (field_mut 1 x/281))
           (if *match*/285
             (if (seq (setfield_ptr 1 x/281 0) 0) 2
               (let (*match*/286 =o (field_mut 1 x/281))
                 (field_imm 0 *match*/286)))
             1))
         0)))
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
  (f/290 =
     (function x/291 : int
       (if (field_int 0 x/291)
         (let (*match*/295 =o (field_mut 1 x/291))
           (if *match*/295 (field_imm 0 *match*/295) 1))
         0)))
  (apply (field_mut 1 (global Toploop!)) "f" f/290))
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
  (f/297 =
     (function r/298 : int
       (let (*match*/300 = (makeblock 0 r/298))
         (catch
           (if *match*/300
             (let (*match*/302 =o (field_mut 0 (field_imm 0 *match*/300)))
               (if *match*/302 (exit 7) 0))
             (exit 7))
          with (7)
           (if (seq (setfield_ptr 0 r/298 0) 0) 1
             (if *match*/300
               (let (*match*/304 =o (field_mut 0 (field_imm 0 *match*/300)))
                 (field_imm 0 *match*/304))
               3))))))
  (apply (field_mut 1 (global Toploop!)) "f" f/297))
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
  (test/308 =
     (function param/311 : int
       (if param/311 (field_imm 0 (field_imm 0 param/311)) 0)))
  (apply (field_mut 1 (global Toploop!)) "test" test/308))
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
  (test/316 =
     (function param/318 : int
       (let (*match*/319 =o (field_mut 0 param/318))
         (if *match*/319 (field_imm 0 (field_imm 0 *match*/319)) 0))))
  (apply (field_mut 1 (global Toploop!)) "test" test/316))
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
  (test/324 =
     (function n/325 : int
       (let
         (*match*/328 =
            (makeblock 0 (makeblock 0 (makemutable 0 (int) 1) [0: 42])))
         (if *match*/328
           (let
             (*match*/329 =a (field_imm 0 *match*/328)
              *match*/331 =o (field_mut 0 (field_imm 0 *match*/329)))
             (if *match*/331 (field_imm 0 (field_imm 1 *match*/329))
               (~ (field_imm 0 (field_imm 1 *match*/329)))))
           3))))
  (apply (field_mut 1 (global Toploop!)) "test" test/324))
val test : 'a -> int = <fun>
|}]
