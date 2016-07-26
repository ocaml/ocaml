(* Check the unboxing *)

(* For concrete types *)
type t1 = A of string [@@ocaml.unboxed];;

let x = A "foo" in
Obj.repr x == Obj.repr (match x with A s -> s)
;;

(* For records *)
type t2 = { f : string } [@@ocaml.unboxed];;

let x = { f = "foo" } in
Obj.repr x == Obj.repr x.f
;;

(* For inline records *)
type t3 = B of { g : string } [@@ocaml.unboxed];;

let x = B { g = "foo" } in
Obj.repr x == Obj.repr (match x with B {g} -> g)
;;

(* Check unboxable types *)
type t4 = C [@@ocaml.unboxed];;  (* no argument *)
type t5 = D of int * string [@@ocaml.unboxed];; (* more than one argument *)
type t5 = E | F [@@ocaml.unboxed];;          (* more than one constructor *)
type t6 = G of int | H [@@ocaml.unboxed];;
type t7 = I of string | J of bool [@@ocaml.unboxed];;

type t8 = { h : bool; i : int } [@@ocaml.unboxed];;  (* more than one field *)
type t9 = K of { j : string; l : int } [@@ocaml.unboxed];;

(* let rec must be rejected *)
type t10 = A of t10 [@@ocaml.unboxed];;
let rec x = A x;;

(* Representation mismatch between module and signature must be rejected *)
module M : sig
  type t = A of string
end = struct
  type t = A of string [@@ocaml.unboxed]
end;;

module N : sig
  type t = A of string [@@ocaml.unboxed]
end = struct
  type t = A of string
end;;

module O : sig
  type t = { f : string }
end = struct
  type t = { f : string } [@@ocaml.unboxed]
end;;

module P : sig
  type t = { f : string } [@@ocaml.unboxed]
end = struct
  type t = { f : string }
end;;

module Q : sig
  type t = A of { f : string }
end = struct
  type t = A of { f : string } [@@ocaml.unboxed]
end;;

module R : sig
  type t = A of { f : string } [@@ocaml.unboxed]
end = struct
  type t = A of { f : string }
end;;


(* Check interference with representation of float arrays. *)
type t11 = L of float [@@ocaml.unboxed];;
let x = Array.make 10 (L 3.14)   (* represented as a flat array *)
and f (a : t11 array) = a.(0)    (* might wrongly assume an array of pointers *)
in assert (f x = L 3.14);;


(* Check for a potential infinite loop in the typing algorithm. *)
type 'a t12 = M of 'a t12 [@@ocaml.unboxed];;
let f (a : int t12 array) = a.(0);;

(* Check for another possible loop *)
type t13 = A : _ t12 -> t13 [@@ocaml.unboxed];;



(* should work *)
type t14;;
type t15 = A of t14 [@@ocaml.unboxed];;

(* should fail *)
type 'a abs;;
type t16 = A : _ abs -> t16 [@@ocaml.unboxed];;

(* should work *)
type t18 = A : _ list abs -> t18 [@@ocaml.unboxed];;

(* should fail because the compiler knows that t is actually float and
   optimizes the record's representation *)
module S : sig
  type t
  type u = { f1 : t; f2 : t }
end = struct
  type t = A of float [@@ocaml.unboxed]
  type u = { f1 : t; f2 : t }
end;;


(* implementing [@@immediate] with [@@ocaml.unboxed]: this works because the
   representation of [t] is [int]
 *)
module T : sig
  type t [@@immediate]
end = struct
  type t = A of int [@@ocaml.unboxed]
end;;
