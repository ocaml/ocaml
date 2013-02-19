(* Exotic OCaml syntax constructs that are not used in the source of *)
(* the OCaml distribution (even in the tests). *)

(* Spaces between the parts of the ?label: token in a typexpr. *)
type t = ? label : int -> int -> int;;

(* Lazy in a pattern. *)
let f x =
  match x with lazy y -> y
;;

(* Spaces between the parts of the ?label: token in a class-type. *)
class c1 =
  (fun ?label:x y -> object end : ? label : int -> int -> object end)
;;

(* type-class annotation in class-expr *)
class c2 = (object end : object end);;

(* virtual object field *)
class virtual c3 = object val virtual x : int end;;
class virtual c4 = object val mutable virtual x : int end;;

(* abstract module type in a signature *)
module type T = sig
  module type U
end;;
