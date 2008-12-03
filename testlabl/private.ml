module Foobar : sig
  type t = private int
end = struct
  type t = int
end;;

module F0 : sig type t = private int end = Foobar;;

let f (x : F0.t) = (x : Foobar.t);; (* fails *)

module F = Foobar;;

let f (x : F.t) = (x : Foobar.t);;

module M = struct type t = <m:int> end;;
module M1 : sig type t = private <m:int; ..> end = M;;
module M2 :  sig type t = private <m:int; ..> end = M1;;
fun (x : M1.t) -> (x : M2.t);; (* fails *)

module M3 : sig type t = private M1.t end = M1;;
fun x -> (x : M3.t :> M1.t);;
fun x -> (x : M3.t :> M.t);;
module M4 : sig type t = private M3.t end = M2;; (* fails *)
module M4 : sig type t = private M3.t end = M;; (* fails *)
module M4 : sig type t = private M3.t end = M1;; (* might be ok *)
module M5 : sig type t = private M1.t end = M3;;
module M6 : sig type t = private < n:int; .. > end = M1;; (* fails *)

module Bar : sig type t = private Foobar.t val f : int -> t end =
  struct type t = int let f (x : int) = (x : t) end;; (* must fail *)

