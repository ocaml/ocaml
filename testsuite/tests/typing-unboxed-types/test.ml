(* TEST
   * expect
*)

(* Check the unboxing *)

(* For concrete types *)
type t1 = A of string [@@ocaml.unboxed];;
[%%expect{|
type t1 = A of string [@@unboxed]
|}];;

let x = A "foo" in
Obj.repr x == Obj.repr (match x with A s -> s)
;;
[%%expect{|
- : bool = true
|}];;

(* For records *)
type t2 = { f : string } [@@ocaml.unboxed];;
[%%expect{|
type t2 = { f : string; } [@@unboxed]
|}];;

let x = { f = "foo" } in
Obj.repr x == Obj.repr x.f
;;
[%%expect{|
- : bool = true
|}];;

(* For inline records *)
type t3 = B of { g : string } [@@ocaml.unboxed];;
[%%expect{|
type t3 = B of { g : string; } [@@unboxed]
|}];;

let x = B { g = "foo" } in
Obj.repr x == Obj.repr (match x with B {g} -> g)
;;
[%%expect{|
- : bool = true
|}];;

(* Check unboxable types *)
type t4 = C [@@ocaml.unboxed];;  (* no argument *)
[%%expect{|
Line 1, characters 0-29:
1 | type t4 = C [@@ocaml.unboxed];;  (* no argument *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because its constructor has no argument.
|}];;
type t5 = D of int * string [@@ocaml.unboxed];; (* more than one argument *)
[%%expect{|
Line 1, characters 0-45:
1 | type t5 = D of int * string [@@ocaml.unboxed];; (* more than one argument *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       its constructor has more than one argument.
|}];;
type t5 = E | F [@@ocaml.unboxed];;          (* more than one constructor *)
[%%expect{|
Line 1, characters 0-33:
1 | type t5 = E | F [@@ocaml.unboxed];;          (* more than one constructor *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because it has more than one constructor.
|}];;
type t6 = G of int | H [@@ocaml.unboxed];;
[%%expect{|
Line 1, characters 0-40:
1 | type t6 = G of int | H [@@ocaml.unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because it has more than one constructor.
|}];;
type t7 = I of string | J of bool [@@ocaml.unboxed];;

type t8 = { h : bool; i : int } [@@ocaml.unboxed];;  (* more than one field *)
[%%expect{|
Line 1, characters 0-51:
1 | type t7 = I of string | J of bool [@@ocaml.unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because it has more than one constructor.
|}];;
type t9 = K of { j : string; l : int } [@@ocaml.unboxed];;
[%%expect{|
Line 1, characters 0-56:
1 | type t9 = K of { j : string; l : int } [@@ocaml.unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       its constructor has more than one field.
|}];;

(* let rec must be rejected *)
type t10 = A of t10 [@@ocaml.unboxed];;
[%%expect{|
type t10 = A of t10 [@@unboxed]
|}];;
let rec x = A x;;
[%%expect{|
Line 1, characters 12-15:
1 | let rec x = A x;;
                ^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

(* Representation mismatch between module and signature must be rejected *)
module M : sig
  type t = A of string
end = struct
  type t = A of string [@@ocaml.unboxed]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A of string [@@ocaml.unboxed]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = A of string [@@unboxed] end
       is not included in
         sig type t = A of string end
       Type declarations do not match:
         type t = A of string [@@unboxed]
       is not included in
         type t = A of string
       Their internal representations differ:
       the first declaration uses unboxed representation.
|}];;

module N : sig
  type t = A of string [@@ocaml.unboxed]
end = struct
  type t = A of string
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A of string
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = A of string end
       is not included in
         sig type t = A of string [@@unboxed] end
       Type declarations do not match:
         type t = A of string
       is not included in
         type t = A of string [@@unboxed]
       Their internal representations differ:
       the second declaration uses unboxed representation.
|}];;

module O : sig
  type t = { f : string }
end = struct
  type t = { f : string } [@@ocaml.unboxed]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { f : string } [@@ocaml.unboxed]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = { f : string; } [@@unboxed] end
       is not included in
         sig type t = { f : string; } end
       Type declarations do not match:
         type t = { f : string; } [@@unboxed]
       is not included in
         type t = { f : string; }
       Their internal representations differ:
       the first declaration uses unboxed representation.
|}];;

module P : sig
  type t = { f : string } [@@ocaml.unboxed]
end = struct
  type t = { f : string }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { f : string }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = { f : string; } end
       is not included in
         sig type t = { f : string; } [@@unboxed] end
       Type declarations do not match:
         type t = { f : string; }
       is not included in
         type t = { f : string; } [@@unboxed]
       Their internal representations differ:
       the second declaration uses unboxed representation.
|}];;

module Q : sig
  type t = A of { f : string }
end = struct
  type t = A of { f : string } [@@ocaml.unboxed]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A of { f : string } [@@ocaml.unboxed]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = A of { f : string; } [@@unboxed] end
       is not included in
         sig type t = A of { f : string; } end
       Type declarations do not match:
         type t = A of { f : string; } [@@unboxed]
       is not included in
         type t = A of { f : string; }
       Their internal representations differ:
       the first declaration uses unboxed representation.
|}];;

module R : sig
  type t = A of { f : string } [@@ocaml.unboxed]
end = struct
  type t = A of { f : string }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A of { f : string }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = A of { f : string; } end
       is not included in
         sig type t = A of { f : string; } [@@unboxed] end
       Type declarations do not match:
         type t = A of { f : string; }
       is not included in
         type t = A of { f : string; } [@@unboxed]
       Their internal representations differ:
       the second declaration uses unboxed representation.
|}];;


(* Check interference with representation of float arrays. *)
type t11 = L of float [@@ocaml.unboxed];;
[%%expect{|
type t11 = L of float [@@unboxed]
|}];;
let x = Array.make 10 (L 3.14)   (* represented as a flat array *)
and f (a : t11 array) = a.(0)    (* might wrongly assume an array of pointers *)
in assert (f x = L 3.14);;
[%%expect{|
- : unit = ()
|}];;


(* Check for a potential infinite loop in the typing algorithm. *)
type 'a t12 = M of 'a t12 [@@ocaml.unboxed];;
[%%expect{|
type 'a t12 = M of 'a t12 [@@unboxed]
|}];;
let f (a : int t12 array) = a.(0);;
[%%expect{|
val f : int t12 array -> int t12 = <fun>
|}];;

(* Check for another possible loop *)
type t13 = A : _ t12 -> t13 [@@ocaml.unboxed];;
[%%expect{|
type t13 = A : 'a t12 -> t13 [@@unboxed]
|}];;


(* should work *)
type t14;;
type t15 = A of t14 [@@ocaml.unboxed];;
[%%expect{|
type t14
type t15 = A of t14 [@@unboxed]
|}];;

(* should fail because the compiler knows that t is actually float and
   optimizes the record's representation *)
module S : sig
  type t
  type u = { f1 : t; f2 : t }
end = struct
  type t = A of float [@@ocaml.unboxed]
  type u = { f1 : t; f2 : t }
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t = A of float [@@ocaml.unboxed]
6 |   type u = { f1 : t; f2 : t }
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = A of float [@@unboxed] type u = { f1 : t; f2 : t; } end
       is not included in
         sig type t type u = { f1 : t; f2 : t; } end
       Type declarations do not match:
         type u = { f1 : t; f2 : t; }
       is not included in
         type u = { f1 : t; f2 : t; }
       Their internal representations differ:
       the first declaration uses unboxed float representation.
|}];;

(* implementing [@@immediate] with [@@ocaml.unboxed]: this works because the
   representation of [t] is [int]
 *)
module T : sig
  type t [@@immediate]
end = struct
  type t = A of int [@@ocaml.unboxed]
end;;
[%%expect{|
module T : sig type t [@@immediate] end
|}];;

(* Another corner case *)
type 'a s
type ('a, 'p) t = private 'a s
type 'a packed = T : ('a, _) t -> 'a packed [@@unboxed]
;;
[%%expect{|
type 'a s
type ('a, 'p) t = private 'a s
type 'a packed = T : ('a, 'b) t -> 'a packed [@@unboxed]
|}];;

(* MPR#7682 *)
type f = {field: 'a. 'a list} [@@unboxed];;
let g = Array.make 10 { field=[] };;
let h = g.(5);;
[%%expect{|
type f = { field : 'a. 'a list; } [@@unboxed]
val g : f array =
  [|{field = []}; {field = []}; {field = []}; {field = []}; {field = []};
    {field = []}; {field = []}; {field = []}; {field = []}; {field = []}|]
val h : f = {field = []}
|}];;

(* Using [@@immediate] information (GPR#1469) *)
type 'a t [@@immediate];;
type u = U : 'a t -> u [@@unboxed];;
[%%expect{|
type 'a t [@@immediate]
type u = U : 'a t -> u [@@unboxed]
|}];;

(* This could not be accepted without using a fixpoint to check unboxed declarations
   (GPR#2188) *)
type ('a, 'b) t = K : 'c -> (bool, 'c) t [@@unboxed]
and t1 = T1 : (bool, int) t -> t1 [@@unboxed]
[%%expect{|
type ('a, 'b) t = K : 'c -> (bool, 'c) t [@@unboxed]
and t1 = T1 : (bool, int) t -> t1 [@@unboxed]
|}];;

(* This real-world example of recursive declaration comes from Markus Mottl
   -- see MPR#7361 *)
type ('a, 'kind) tree =
  | Root : { mutable value : 'a; mutable rank : int } -> ('a, [ `root ]) tree
  | Inner : { mutable parent : 'a node } -> ('a, [ `inner ]) tree
and 'a node = Node : ('a, _) tree -> 'a node [@@ocaml.unboxed]
[%%expect{|
type ('a, 'kind) tree =
    Root : { mutable value : 'a; mutable rank : int;
    } -> ('a, [ `root ]) tree
  | Inner : { mutable parent : 'a node; } -> ('a, [ `inner ]) tree
and 'a node = Node : ('a, 'b) tree -> 'a node [@@unboxed]
|}];;
