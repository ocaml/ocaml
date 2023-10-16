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
  (f/280 =
     (function x/282 : int
       (if (field_int 0 x/282)
         (let (*match*/286 =o (field_mut 1 x/282))
           (if *match*/286
             (if (seq (setfield_ptr 1 x/282 0) 0) 2
               (let (*match*/287 =o (field_mut 1 x/282))
                 (field_imm 0 *match*/287)))
             1))
         0)))
  (apply (field_mut 1 (global Toploop!)) "f" f/280))
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
  (f/291 =
     (function x/292 : int
       (if (field_int 0 x/292)
         (let (*match*/296 =o (field_mut 1 x/292))
           (if *match*/296 (field_imm 0 *match*/296) 1))
         0)))
  (apply (field_mut 1 (global Toploop!)) "f" f/291))
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
  (f/298 =
     (function r/299 : int
       (let (*match*/301 = (makeblock 0 r/299))
         (catch
           (if *match*/301
             (let (*match*/303 =o (field_mut 0 (field_imm 0 *match*/301)))
               (if *match*/303 (exit 7) 0))
             (exit 7))
          with (7)
           (if (seq (setfield_ptr 0 r/299 0) 0) 1
             (if *match*/301
               (let (*match*/305 =o (field_mut 0 (field_imm 0 *match*/301)))
                 (field_imm 0 *match*/305))
               3))))))
  (apply (field_mut 1 (global Toploop!)) "f" f/298))
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
  (test/309 =
     (function param/312 : int
       (if param/312 (field_imm 0 (field_imm 0 param/312)) 0)))
  (apply (field_mut 1 (global Toploop!)) "test" test/309))
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
  (test/317 =
     (function param/319 : int
       (let (*match*/320 =o (field_mut 0 param/319))
         (if *match*/320 (field_imm 0 (field_imm 0 *match*/320)) 0))))
  (apply (field_mut 1 (global Toploop!)) "test" test/317))
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
  (test/325 =
     (function n/326 : int
       (let
         (*match*/329 =
            (makeblock 0 (makeblock 0 (makemutable 0 (int) 1) [0: 42])))
         (if *match*/329
           (let
             (*match*/330 =a (field_imm 0 *match*/329)
              *match*/332 =o (field_mut 0 (field_imm 0 *match*/330)))
             (if *match*/332 (field_imm 0 (field_imm 1 *match*/330))
               (~ (field_imm 0 (field_imm 1 *match*/330)))))
           3))))
  (apply (field_mut 1 (global Toploop!)) "test" test/325))
val test : 'a -> int = <fun>
|}]
