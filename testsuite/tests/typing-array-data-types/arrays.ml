
module Foo = struct

  type t = [| mutable int |]

end
;;

(* Warning 40 *)

let warning (x : Foo.t) = x.(0) (* WARNING: .() is not in scope *)
;;

open Foo

(* Get *)

let get_foo (x : Foo.t) = x.(0)
;;

let get_foo2 x = x.Foo.(0)
;;

(* Set *)

let set_foo (x : Foo.t) = x.(0) <- 9
;;

let set_foo2 x = x.Foo.(0) <- 9
;;

(* Array literals *)

let lit_foo : Foo.t = [| 1; 2; 3 |]
;;

let lit_foo2 = Foo.[| 1; 2; 3 |]
;;

(* Array length *)

let length = lit_foo.length

(* Array comprehensions *)

let comp_foo : Foo.t = [| x for x = 0 to 10 |]
;;

let comp_foo2 = Foo.[| x * x for x = 0 to 10 |]
;;

let comp_down_foo : Foo.t = [| x for x = 10 downto 0 |]
;;

(* Empty array comprehensions *)

let empty_foo : Foo.t = [| (print_endline "Bad"; x) for x = 1 to 0 |]
;;

let empty_down_foo : Foo.t = [| (print_endline "Bad"; x) for x = 10 downto 11 |]
;;

(* Immutable arrays are immutable *)

type bar = [| int |]
;;

let set_bar (x : bar) = x.(0) <- 9 (* ERROR: bar is not mutable *)
;;

