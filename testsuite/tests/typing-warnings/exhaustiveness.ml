(* TEST
 flags = " -w +A -strict-sequence ";
 expect;
*)

let f = function
    None, None -> 1
  | Some _, Some _ -> 2;;
[%%expect {|
Lines 1-3, characters 8-23:
1 | ........function
2 |     None, None -> 1
3 |   | Some _, Some _ -> 2..
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "(None, Some _)"

val f : 'a option * 'b option -> int = <fun>
|}]

type _ t =
  A : int t | B : bool t | C : char t | D : float t
type (_,_,_,_) u = U : (int, int, int, int) u
type v = E | F | G
;;
[%%expect {|
type _ t = A : int t | B : bool t | C : char t | D : float t
type (_, _, _, _) u = U : (int, int, int, int) u
type v = E | F | G
|}]

(* Unused cases *)
let f (x : int t) = match x with A -> 1 | _ -> 2;; (* warn *)
[%%expect {|
Line 1, characters 20-48:
1 | let f (x : int t) = match x with A -> 1 | _ -> 2;; (* warn *)
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 4 [fragile-match]: this pattern-matching is fragile.
  It will remain exhaustive when constructors are added to type "t".

Line 1, characters 42-43:
1 | let f (x : int t) = match x with A -> 1 | _ -> 2;; (* warn *)
                                              ^
Warning 56 [unreachable-case]: this match case is unreachable.
  Consider replacing it with a refutation case "<pat> -> ."

val f : int t -> int = <fun>
|}]

let f (x : unit t option) = match x with None -> 1 | _ -> 2 ;; (* warn? *)
[%%expect {|
Line 1, characters 53-54:
1 | let f (x : unit t option) = match x with None -> 1 | _ -> 2 ;; (* warn? *)
                                                         ^
Warning 56 [unreachable-case]: this match case is unreachable.
  Consider replacing it with a refutation case "<pat> -> ."

val f : unit t option -> int = <fun>
|}]

let f (x : unit t option) = match x with None -> 1 | Some _ -> 2 ;; (* warn *)
[%%expect {|
Line 1, characters 53-59:
1 | let f (x : unit t option) = match x with None -> 1 | Some _ -> 2 ;; (* warn *)
                                                         ^^^^^^
Warning 56 [unreachable-case]: this match case is unreachable.
  Consider replacing it with a refutation case "<pat> -> ."

val f : unit t option -> int = <fun>
|}]

let f (x : int t option) = match x with None -> 1 | _ -> 2;;
[%%expect {|
val f : int t option -> int = <fun>
|}]

let f (x : int t option) = match x with None -> 1;; (* warn *)
[%%expect {|
Line 1, characters 27-49:
1 | let f (x : int t option) = match x with None -> 1;; (* warn *)
                               ^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Some A"

val f : int t option -> int = <fun>
|}]

(* Example with record, type, single case *)

type 'a box = Box of 'a
type 'a pair = {left: 'a; right: 'a};;
[%%expect {|
type 'a box = Box of 'a
type 'a pair = { left : 'a; right : 'a; }
|}]

let f : (int t box pair * bool) option -> unit = function None -> ();;
[%%expect {|
Line 1, characters 49-68:
1 | let f : (int t box pair * bool) option -> unit = function None -> ();;
                                                     ^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
    "Some ({left=Box A; right=Box A}, _)"

val f : (int t box pair * bool) option -> unit = <fun>
|}]

let f : (string t box pair * bool) option -> unit = function None -> ();;
[%%expect {|
val f : (string t box pair * bool) option -> unit = <fun>
|}]

let f = function {left=Box 0; _ } -> ();;
[%%expect {|
Line 1, characters 8-39:
1 | let f = function {left=Box 0; _ } -> ();;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "{left=Box 1; _ }"

val f : int box pair -> unit = <fun>
|}]

let f = function {left=Box 0;right=Box 1} -> ();;
[%%expect {|
Line 1, characters 8-47:
1 | let f = function {left=Box 0;right=Box 1} -> ();;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "{left=Box 0; right=Box 0}"

val f : int box pair -> unit = <fun>
|}]

(* Examples from ML2015 paper *)

type _ t =
  | Int : int t
  | Bool : bool t
;;
[%%expect {|
type _ t = Int : int t | Bool : bool t
|}]

let f : type a. a t -> a = function
  | Int -> 1
  | Bool -> true
;;
[%%expect {|
val f : 'a t -> 'a = <fun>
|}]

let g : int t -> int = function
  | Int -> 1
;;
[%%expect {|
val g : int t -> int = <fun>
|}]

let h : type a. a t -> a t -> bool =
  fun x y -> match x, y with
  | Int, Int -> true
  | Bool, Bool -> true
;;
[%%expect {|
val h : 'a t -> 'a t -> bool = <fun>
|}]

type (_, _) cmp =
 | Eq : ('a, 'a) cmp
 | Any: ('a, 'b) cmp
module A : sig type a type b val eq : (a, b) cmp end
  = struct type a type b = a let eq = Eq end
;;
[%%expect {|
type (_, _) cmp = Eq : ('a, 'a) cmp | Any : ('a, 'b) cmp
module A : sig type a type b val eq : (a, b) cmp end
|}]

let f : (A.a, A.b) cmp -> unit = function Any -> ()
;;
[%%expect {|
Line 1, characters 33-51:
1 | let f : (A.a, A.b) cmp -> unit = function Any -> ()
                                     ^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Eq"

val f : (A.a, A.b) cmp -> unit = <fun>
|}]

let deep : char t option -> char =
  function None -> 'c'
;;
[%%expect {|
val deep : char t option -> char = <fun>
|}]

type zero = Zero
type _ succ = Succ
;;
[%%expect {|
type zero = Zero
type _ succ = Succ
|}]

type (_,_,_) plus =
  | Plus0 : (zero, 'a, 'a) plus
  | PlusS : ('a, 'b, 'c) plus ->
       ('a succ, 'b, 'c succ) plus
;;
[%%expect {|
type (_, _, _) plus =
    Plus0 : (zero, 'a, 'a) plus
  | PlusS : ('a, 'b, 'c) plus -> ('a succ, 'b, 'c succ) plus
|}]

let trivial : (zero succ, zero, zero) plus option -> bool =
  function None -> false
;;
[%%expect {|
val trivial : (zero succ, zero, zero) plus option -> bool = <fun>
|}]

let easy : (zero, zero succ, zero) plus option -> bool =
  function None -> false
;;
[%%expect {|
val easy : (zero, zero succ, zero) plus option -> bool = <fun>
|}]

let harder : (zero succ, zero succ, zero succ) plus option -> bool =
  function None -> false
;;
[%%expect {|
Line 2, characters 2-24:
2 |   function None -> false
      ^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Some (PlusS _)"

val harder : (zero succ, zero succ, zero succ) plus option -> bool = <fun>
|}]

let harder : (zero succ, zero succ, zero succ) plus option  -> bool =
  function None -> false | Some (PlusS _) -> .
;;
[%%expect {|
val harder : (zero succ, zero succ, zero succ) plus option -> bool = <fun>
|}]

let inv_zero : type a b c d. (a,b,c) plus -> (c,d,zero) plus -> bool =
  fun p1 p2 ->
    match p1, p2 with
    | Plus0, Plus0 -> true
;;
[%%expect {|
val inv_zero : ('a, 'b, 'c) plus -> ('c, 'd, zero) plus -> bool = <fun>
|}]


(* Empty match *)

type _ t = Int : int t;;
[%%expect {|
type _ t = Int : int t
|}]

let f (x : bool t) = match x with _ -> . ;; (* ok *)
[%%expect {|
val f : bool t -> 'a = <fun>
|}]


(* trefis in PR#6437 *)

let f () = match None with _ -> .;; (* error *)
[%%expect {|
Line 1, characters 27-28:
1 | let f () = match None with _ -> .;; (* error *)
                               ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: "_"
|}]

let g () = match None with _ -> () | exception _ -> .;; (* error *)
[%%expect {|
Line 1, characters 47-48:
1 | let g () = match None with _ -> () | exception _ -> .;; (* error *)
                                                   ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: "_"
|}]

let h () = match None with _ -> .  | exception _ -> .;; (* error *)
[%%expect {|
Line 1, characters 27-28:
1 | let h () = match None with _ -> .  | exception _ -> .;; (* error *)
                               ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: "_"
|}]

let f x = match x with _ -> () | None -> .;; (* do not warn *)
[%%expect {|
val f : 'a option -> unit = <fun>
|}]

(* #7059, all clauses guarded *)

let f x y = match 1 with 1 when x = y -> 1;;
[%%expect {|
Line 1, characters 12-42:
1 | let f x y = match 1 with 1 when x = y -> 1;;
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  All clauses in this pattern-matching are guarded.

val f : 'a -> 'a -> int = <fun>
|}]

(* #7504, Example with no constraints on a record *)
let f = function {contents=_}, 0 -> 0;;
[%%expect {|
Line 1, characters 8-37:
1 | let f = function {contents=_}, 0 -> 0;;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "({ _ }, 1)"

val f : 'a ref * int -> int = <fun>
|}]

(* inexhaustive however some guarded clause might match *)
let f = function
  | None -> ()
  | Some x when x > 0 -> ()
  | Some x when x <= 0 -> ()
;;
[%%expect {|
Lines 1-4, characters 8-28:
1 | ........function
2 |   | None -> ()
3 |   | Some x when x > 0 -> ()
4 |   | Some x when x <= 0 -> ()
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
    "Some _"
    (However, some guarded clause may match this value.)

val f : int option -> unit = <fun>
|}]

(* in the single-row case we can generate more compact witnesses *)
module Single_row_optim = struct
type t = A | B

(* This synthetic program is representative of user-written programs
   that try to distinguish the cases "only A" and "at least one B"
   while avoiding a fragile pattern-matching (using just _ in the last
   row would be fragile).

   It is a "single row" program from the point of view of
   exhaustiveness checking because the first row is subsumed by the
   second and thus removed by the [get_mins] preprocessing of
   Parmatch.

   With the single-row optimization implemented in the compiler, it
   generates a single counter-example that contains
   or-patterns. Without this optimization, it would generate 2^(N-1)
   counter-examples (here N=4 so 8), one for each possible expansion
   of the or-patterns.
*)
let non_exhaustive : t * t * t * t -> unit = function
| A, A, A, A -> ()
| (A|B), (A|B), (A|B), A (*missing B here*) -> ()
end;;
[%%expect {|
Lines 20-22, characters 45-49:
20 | .............................................function
21 | | A, A, A, A -> ()
22 | | (A|B), (A|B), (A|B), A (*missing B here*) -> ()
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "((A|B), (A|B), (A|B), B)"

module Single_row_optim :
  sig type t = A | B val non_exhaustive : t * t * t * t -> unit end
|}]
