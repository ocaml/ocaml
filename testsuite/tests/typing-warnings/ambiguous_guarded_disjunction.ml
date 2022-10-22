(* TEST
   flags = " -w +A -strict-sequence "
   * expect
*)

(* Ignore OCAMLRUNPARAM=b to be reproducible *)
Printexc.record_backtrace false;;
[%%expect {|
- : unit = ()
|}]

type expr = Val of int | Rest;;
[%%expect {|
type expr = Val of int | Rest
|}]

let ambiguous_typical_example = function
  | ((Val x, _) | (_, Val x)) when x < 0 -> ()
  | (_, Rest) -> ()
  | (_, Val x) ->
      (* the reader might expect *)
      assert (x >= 0);
      (* to hold here, but it is wrong! *)
      ()
;;
[%%expect {|
Line 2, characters 4-29:
2 |   | ((Val x, _) | (_, Val x)) when x < 0 -> ()
        ^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 57 [ambiguous-var-in-pattern-guard]: Ambiguous or-pattern variables under guard;
variable x appears in different places in different or-pattern alternatives.
Only the first match will be used to evaluate the guard expression.
(See manual section 13.5)
val ambiguous_typical_example : expr * expr -> unit = <fun>
|}]

let fails = ambiguous_typical_example (Val 2, Val (-1))
;;
[%%expect {|
Exception: Assert_failure ("", 6, 6).
|}]

let not_ambiguous__no_orpat = function
  | Some x when x > 0 -> ()
  | Some _ -> ()
  | None -> ()
;;
[%%expect {|
val not_ambiguous__no_orpat : int option -> unit = <fun>
|}]

let not_ambiguous__no_guard = function
  | `A -> ()
  | (`B | `C) -> ()
;;
[%%expect {|
val not_ambiguous__no_guard : [< `A | `B | `C ] -> unit = <fun>
|}]

let not_ambiguous__no_patvar_in_guard b = function
  | (`B x | `C x) when b -> ignore x
  | _ -> ()
;;
[%%expect {|
val not_ambiguous__no_patvar_in_guard :
  bool -> [> `B of 'a | `C of 'a ] -> unit = <fun>
|}]

let not_ambiguous__disjoint_cases = function
  | (`B x | `C x) when x -> ()
  | _ -> ()
;;
[%%expect {|
val not_ambiguous__disjoint_cases : [> `B of bool | `C of bool ] -> unit =
  <fun>
|}]

(* the curious (..., _, Some _) | (..., Some _, _) device used in
   those tests serves to avoid warning 12 (this sub-pattern
   is unused), by making sure that, even if the two sides of the
   disjunction overlap, none is fully included in the other. *)
let not_ambiguous__prefix_variables = function
  | (`B (x, _, Some y) | `B (x, Some y, _)) when x -> ignore y
  | _ -> ()
;;
[%%expect {|
val not_ambiguous__prefix_variables :
  [> `B of bool * 'a option * 'a option ] -> unit = <fun>
|}]

let ambiguous__y = function
  | (`B (x, _, Some y) | `B (x, Some y, _)) when y -> ignore x
  | _ -> ()
;;
[%%expect {|
Line 2, characters 4-43:
2 |   | (`B (x, _, Some y) | `B (x, Some y, _)) when y -> ignore x
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 57 [ambiguous-var-in-pattern-guard]: Ambiguous or-pattern variables under guard;
variable y appears in different places in different or-pattern alternatives.
Only the first match will be used to evaluate the guard expression.
(See manual section 13.5)
val ambiguous__y : [> `B of 'a * bool option * bool option ] -> unit = <fun>
|}]

(* it should be understood that the ambiguity warning only protects
     (p | q) when guard -> ...
   it will never warn on
     (p | q) -> if guard ...
   This is not a limitation. The point is that people have an
   intuitive understanding of [(p | q) when guard -> ...] that differs
   from the reality, while there is no such issue with
   [(p | q) -> if guard ...].
*)
let not_ambiguous__rhs_not_protected = function
  | (`B (x, _, Some y) | `B (x, Some y, _)) -> if y then ignore x else ()
  | _ -> ()
;;
[%%expect {|
val not_ambiguous__rhs_not_protected :
  [> `B of 'a * bool option * bool option ] -> unit = <fun>
|}]

let ambiguous__x_y = function
  | (`B (x, _, Some y) | `B (x, Some y, _)) when x < y -> ()
  | _ -> ()
;;
[%%expect {|
Line 2, characters 4-43:
2 |   | (`B (x, _, Some y) | `B (x, Some y, _)) when x < y -> ()
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 57 [ambiguous-var-in-pattern-guard]: Ambiguous or-pattern variables under guard;
variable y appears in different places in different or-pattern alternatives.
Only the first match will be used to evaluate the guard expression.
(See manual section 13.5)
val ambiguous__x_y : [> `B of 'a * 'a option * 'a option ] -> unit = <fun>
|}]

let ambiguous__x_y_z = function
  | (`B (x, z, Some y) | `B (x, Some y, z)) when x < y || Some x = z -> ()
  | _ -> ()
;;
[%%expect {|
Line 2, characters 4-43:
2 |   | (`B (x, z, Some y) | `B (x, Some y, z)) when x < y || Some x = z -> ()
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 57 [ambiguous-var-in-pattern-guard]: Ambiguous or-pattern variables under guard;
variables y, z appear in different places in different or-pattern alternatives.
Only the first match will be used to evaluate the guard expression.
(See manual section 13.5)
val ambiguous__x_y_z : [> `B of 'a * 'a option * 'a option ] -> unit = <fun>
|}]

let not_ambiguous__disjoint_in_depth = function
  | `A (`B x | `C x) when x -> ()
  | _ -> ()
;;
[%%expect {|
val not_ambiguous__disjoint_in_depth :
  [> `A of [> `B of bool | `C of bool ] ] -> unit = <fun>
|}]

let not_ambiguous__prefix_variables_in_depth = function
  | `A (`B (x, `C1) | `B (x, `C2)) when x -> ()
  | _ -> ()
;;
[%%expect {|
val not_ambiguous__prefix_variables_in_depth :
  [> `A of [> `B of bool * [> `C1 | `C2 ] ] ] -> unit = <fun>
|}]

let ambiguous__in_depth = function
  | `A (`B (Some x, _) | `B (_, Some x)) when x -> ()
  | _ -> ()
;;
[%%expect {|
Line 2, characters 4-40:
2 |   | `A (`B (Some x, _) | `B (_, Some x)) when x -> ()
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 57 [ambiguous-var-in-pattern-guard]: Ambiguous or-pattern variables under guard;
variable x appears in different places in different or-pattern alternatives.
Only the first match will be used to evaluate the guard expression.
(See manual section 13.5)
val ambiguous__in_depth :
  [> `A of [> `B of bool option * bool option ] ] -> unit = <fun>
|}]

let not_ambiguous__several_orpats = function
  | `A ((`B (x, Some _, _) | `B (x, _, Some _)),
        (`C (y, Some _, _) | `C (y, _, Some _)),
        (`D1 (_, z, Some _, _) | `D2 (_, z, _, Some _))) when x < y && x < z ->
      ()
  | _ -> ()
;;
[%%expect {|
val not_ambiguous__several_orpats :
  [> `A of
       [> `B of 'a * 'b option * 'c option ] *
       [> `C of 'a * 'd option * 'e option ] *
       [> `D1 of 'f * 'a * 'g option * 'h | `D2 of 'i * 'a * 'j * 'k option ]
  ] -> unit = <fun>
|}]

let ambiguous__first_orpat = function
  | `A ((`B (Some x, _) | `B (_, Some x)),
        (`C (Some y, Some _, _) | `C (Some y, _, Some _))) when x < y -> ()
  | _ -> ()
;;
[%%expect {|
Lines 2-3, characters 4-58:
2 | ....`A ((`B (Some x, _) | `B (_, Some x)),
3 |         (`C (Some y, Some _, _) | `C (Some y, _, Some _))).................
Warning 57 [ambiguous-var-in-pattern-guard]: Ambiguous or-pattern variables under guard;
variable x appears in different places in different or-pattern alternatives.
Only the first match will be used to evaluate the guard expression.
(See manual section 13.5)
val ambiguous__first_orpat :
  [> `A of
       [> `B of 'a option * 'a option ] *
       [> `C of 'a option * 'b option * 'c option ] ] ->
  unit = <fun>
|}]

let ambiguous__second_orpat = function
  | `A ((`B (Some x, Some _, _) | `B (Some x, _, Some _)),
        (`C (Some y, _) | `C (_, Some y))) when x < y -> ()
  | _ -> ()
;;
[%%expect {|
Lines 2-3, characters 4-42:
2 | ....`A ((`B (Some x, Some _, _) | `B (Some x, _, Some _)),
3 |         (`C (Some y, _) | `C (_, Some y))).................
Warning 57 [ambiguous-var-in-pattern-guard]: Ambiguous or-pattern variables under guard;
variable y appears in different places in different or-pattern alternatives.
Only the first match will be used to evaluate the guard expression.
(See manual section 13.5)
val ambiguous__second_orpat :
  [> `A of
       [> `B of 'a option * 'b option * 'c option ] *
       [> `C of 'a option * 'a option ] ] ->
  unit = <fun>
|}]

(* check that common prefixes work as expected *)
let not_ambiguous__pairs = function
  | (x, Some _, _) | (x, _, Some _) when x -> ()
  | _ -> ()
;;
[%%expect {|
val not_ambiguous__pairs : bool * 'a option * 'b option -> unit = <fun>
|}]

let not_ambiguous__vars =
  begin[@warning "-12"] function
  | (x | x) when x -> ()
  | _ -> ()
  end
;;
[%%expect {|
val not_ambiguous__vars : bool -> unit = <fun>
|}]

let not_ambiguous__as p = function
  | (([], _) as x | ((_, []) as x)) when p x -> ()
  | _ -> ()
;;
[%%expect {|
val not_ambiguous__as :
  ('a list * 'b list -> bool) -> 'a list * 'b list -> unit = <fun>
|}]

let not_ambiguous__as_var p = function
  | (([], _) as x | x) when p x -> ()
  | _ -> ()
;;
[%%expect {|
val not_ambiguous__as_var : ('a list * 'b -> bool) -> 'a list * 'b -> unit =
  <fun>
|}]

let not_ambiguous__var_as p = function
  | (x, Some _, _) | (([], _) as x, _, Some _) when p x -> ()
  | _ -> ()
;;
[%%expect {|
val not_ambiguous__var_as :
  ('a list * 'b -> bool) -> ('a list * 'b) * 'c option * 'd option -> unit =
  <fun>
|}]

let not_ambiguous__lazy = function
  | (([], _), lazy x) | ((_, []), lazy x) when x -> ()
  | _ -> ()
;;
[%%expect {|
val not_ambiguous__lazy : ('a list * 'b list) * bool lazy_t -> unit = <fun>
|}]

type t = A of int * int option * int option | B;;
[%%expect {|
type t = A of int * int option * int option | B
|}]

let not_ambiguous__constructor = function
  | A (x, Some _, _) | A (x, _, Some _) when x > 0 -> ()
  | A _ | B -> ()
;;
[%%expect {|
val not_ambiguous__constructor : t -> unit = <fun>
|}]

type amoi = Z of int | Y of int * int  | X of amoi * amoi
;;
[%%expect {|
type amoi = Z of int | Y of int * int | X of amoi * amoi
|}]

let ambiguous__amoi a = match a with
| X (Z x,Y (y,0))
| X (Z y,Y (x,_))
 when x+y > 0 -> 0
| X _|Y _|Z _ -> 1
;;
[%%expect {|
Lines 2-3, characters 2-17:
2 | ..X (Z x,Y (y,0))
3 | | X (Z y,Y (x,_))
Warning 57 [ambiguous-var-in-pattern-guard]: Ambiguous or-pattern variables under guard;
variables x, y appear in different places in different or-pattern alternatives.
Only the first match will be used to evaluate the guard expression.
(See manual section 13.5)
val ambiguous__amoi : amoi -> int = <fun>
|}]

module type S = sig val b : bool end
;;
[%%expect {|
module type S = sig val b : bool end
|}]

let ambiguous__module_variable x b =  match x with
  | (module M:S),_,(1,_)
  | _,(module M:S),(_,1) when M.b && b -> 1
  | _ -> 2
;;
[%%expect {|
Lines 2-3, characters 4-24:
2 | ....(module M:S),_,(1,_)
3 |   | _,(module M:S),(_,1)...................
Warning 57 [ambiguous-var-in-pattern-guard]: Ambiguous or-pattern variables under guard;
variable M appears in different places in different or-pattern alternatives.
Only the first match will be used to evaluate the guard expression.
(See manual section 13.5)
val ambiguous__module_variable :
  (module S) * (module S) * (int * int) -> bool -> int = <fun>
|}]

let not_ambiguous__module_variable x b =  match x with
  | (module M:S),_,(1,_)
  | _,(module M:S),(_,1) when b -> 1
  | _ -> 2
;;
[%%expect {|
Line 2, characters 12-13:
2 |   | (module M:S),_,(1,_)
                ^
Warning 60 [unused-module]: unused module M.
val not_ambiguous__module_variable :
  (module S) * (module S) * (int * int) -> bool -> int = <fun>
|}]

(* Mixed case *)

type t2 = A of int * int | B of int * int
;;
[%%expect {|
type t2 = A of int * int | B of int * int
|}]

let ambiguous_xy_but_not_ambiguous_z g = function
  | A (x as z,(0 as y))|A (0 as y as z,x)|B (x,(y as z)) when g x (y+z) -> 1
  | _ -> 2
;;
[%%expect {|
Line 2, characters 4-5:
2 |   | A (x as z,(0 as y))|A (0 as y as z,x)|B (x,(y as z)) when g x (y+z) -> 1
        ^
Warning 41 [ambiguous-name]: A belongs to several types: t2 t
The first one was selected. Please disambiguate if this is wrong.
Lines 1-3, characters 41-10:
1 | .........................................function
2 |   | A (x as z,(0 as y))|A (0 as y as z,x)|B (x,(y as z)) when g x (y+z) -> 1
3 |   | _ -> 2
Warning 4 [fragile-match]: this pattern-matching is fragile.
It will remain exhaustive when constructors are added to type t2.
Line 2, characters 4-56:
2 |   | A (x as z,(0 as y))|A (0 as y as z,x)|B (x,(y as z)) when g x (y+z) -> 1
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 57 [ambiguous-var-in-pattern-guard]: Ambiguous or-pattern variables under guard;
variables x, y appear in different places in different or-pattern alternatives.
Only the first match will be used to evaluate the guard expression.
(See manual section 13.5)
val ambiguous_xy_but_not_ambiguous_z : (int -> int -> bool) -> t2 -> int =
  <fun>
|}, Principal{|
Line 2, characters 4-5:
2 |   | A (x as z,(0 as y))|A (0 as y as z,x)|B (x,(y as z)) when g x (y+z) -> 1
        ^
Warning 41 [ambiguous-name]: A belongs to several types: t2 t
The first one was selected. Please disambiguate if this is wrong.
Line 2, characters 24-25:
2 |   | A (x as z,(0 as y))|A (0 as y as z,x)|B (x,(y as z)) when g x (y+z) -> 1
                            ^
Warning 41 [ambiguous-name]: A belongs to several types: t2 t
The first one was selected. Please disambiguate if this is wrong.
Line 2, characters 42-43:
2 |   | A (x as z,(0 as y))|A (0 as y as z,x)|B (x,(y as z)) when g x (y+z) -> 1
                                              ^
Warning 41 [ambiguous-name]: B belongs to several types: t2 t
The first one was selected. Please disambiguate if this is wrong.
Lines 1-3, characters 41-10:
1 | .........................................function
2 |   | A (x as z,(0 as y))|A (0 as y as z,x)|B (x,(y as z)) when g x (y+z) -> 1
3 |   | _ -> 2
Warning 4 [fragile-match]: this pattern-matching is fragile.
It will remain exhaustive when constructors are added to type t2.
Line 2, characters 4-56:
2 |   | A (x as z,(0 as y))|A (0 as y as z,x)|B (x,(y as z)) when g x (y+z) -> 1
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 57 [ambiguous-var-in-pattern-guard]: Ambiguous or-pattern variables under guard;
variables x, y appear in different places in different or-pattern alternatives.
Only the first match will be used to evaluate the guard expression.
(See manual section 13.5)
val ambiguous_xy_but_not_ambiguous_z : (int -> int -> bool) -> t2 -> int =
  <fun>
|}]

(* Regression test against an erroneous simplification of the algorithm

   One cannot compute the stable variable of the first row of a matrix
   after its simplification and before splitting the
   submatrices. Indeed, further splits on the submatrices may reveal
   that some rows of this first column belong to disjoint submatrices,
   and thus that the variables are more stable than is visible when
   looking at the full column.
*)
let not_ambiguous__as_disjoint_on_second_column_split = function
| ((Some a, (1 as b)) | (Some b, (2 as a))) when a = 0 -> ignore a; ignore b
| _ -> ()
;;
[%%expect {|
val not_ambiguous__as_disjoint_on_second_column_split :
  int option * int -> unit = <fun>
|}]

(* we check for the ambiguous case first, so there
   is no warning *)
let solved_ambiguity_typical_example = function
  | (Val x, Val y) ->
      if x < 0 || y < 0
      then ()
      else ()
  | ((Val x, _) | (_, Val x)) when x < 0 -> ()
  | (_, Rest) -> ()
  | (_, Val x) ->
      (* the reader can expect *)
      assert (x >= 0);
      (* to hold here. *)
      ()
;;
[%%expect {|
val solved_ambiguity_typical_example : expr * expr -> unit = <fun>
|}]

(* if the check for the ambiguous case is guarded,
   there is still a warning *)
let guarded_ambiguity = function
  | (Val x, Val y) when x < 0 || y < 0 -> ()
  | ((Val y, _) | (_, Val y)) when y < 0 -> ()
  | (_, Rest) -> ()
  | (_, Val x) ->
      (* the reader can expect *)
      assert (x >= 0);
      (* to hold here. *)
      ()
;;
[%%expect {|
Line 3, characters 4-29:
3 |   | ((Val y, _) | (_, Val y)) when y < 0 -> ()
        ^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 57 [ambiguous-var-in-pattern-guard]: Ambiguous or-pattern variables under guard;
variable y appears in different places in different or-pattern alternatives.
Only the first match will be used to evaluate the guard expression.
(See manual section 13.5)
val guarded_ambiguity : expr * expr -> unit = <fun>
|}]

(* see GPR#1552 *)
type a = A1 | A2;;
[%%expect {|
type a = A1 | A2
|}]

type 'a alg =
  | Val of 'a
  | Binop of 'a alg * 'a alg;;
[%%expect {|
type 'a alg = Val of 'a | Binop of 'a alg * 'a alg
|}]

let cmp (pred : a -> bool) (x : a alg) (y : a alg) =
  match x, y with
  | Val A1, Val A1 -> ()
  | ((Val x, _) | (_, Val x)) when pred x -> ()
  (* below: silence exhaustiveness/fragility warnings *)
  | (Val (A1 | A2) | Binop _), _ -> ()
;;
[%%expect {|
Line 4, characters 4-29:
4 |   | ((Val x, _) | (_, Val x)) when pred x -> ()
        ^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 57 [ambiguous-var-in-pattern-guard]: Ambiguous or-pattern variables under guard;
variable x appears in different places in different or-pattern alternatives.
Only the first match will be used to evaluate the guard expression.
(See manual section 13.5)
val cmp : (a -> bool) -> a alg -> a alg -> unit = <fun>
|}]

type a = A1;;
[%%expect {|
type a = A1
|}]

type 'a alg =
  | Val of 'a
  | Binop of 'a alg * 'a alg;;
[%%expect {|
type 'a alg = Val of 'a | Binop of 'a alg * 'a alg
|}]

let cmp (pred : a -> bool) (x : a alg) (y : a alg) =
  match x, y with
  | Val A1, Val A1 -> ()
  | ((Val x, _) | (_, Val x)) when pred x -> ()
  (* below: silence exhaustiveness/fragility warnings *)
  | (Val A1 | Binop _), _ -> ()
;;
[%%expect {|
val cmp : (a -> bool) -> a alg -> a alg -> unit = <fun>
|}]
