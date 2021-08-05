(* TEST
   flags = "-dparsetree"
   * toplevel *)

(* Using a toplevel test and not an expect test, because the locs get shifted
   by the expect blocks and the output is therefore not stable. *)

(* Attributes *)

module type S = sig end [@attr payload];;


module M = struct end [@attr payload];;

type t = int [@attr payload];;

3 [@attr payload];;

exception Exn [@@attr payload];;

(* Functors *)

module type F = functor (A : S) (B : S) -> sig end;;

module F = functor (A : S) (B : S) -> struct end;;

(* with type *)

module type S1 = sig type t end;;

module type T1 = S1 with type t = int;;

module type T1 = S1 with type t := int;;

(* Constrained bindings *)

let x : int = 3;;

let x : type a. a -> a = fun x -> x;;

let _ = object
  method x : type a. a -> a =
    fun x -> x
end;;

(* Punning. *)

let x contents = { contents };;

let x = { contents : int = 3 };;

let x contents = { contents : int };;

let x = function { contents } -> contents;;

let x = function { contents : int } -> contents;;

let x = function { contents : int = i } -> i;;

let _ =
  object val foo = 12 method x foo = {< foo >} end
;;

(* Local open *)

let x = M.{ contents = 3 };;

let x = M.[ 3; 4 ];;

let x = M.( 3; 4 );;

(* Indexing operators *)

  (* some prerequisites. *)

let ( .@() ) x y = x + y
let ( .@()<- ) x y z = x + y + z
let ( .%.{} ) x y = x + y
let ( .%.{}<- ) x y z = x + y + z
let ( .%.[] ) x y = x + y
let ( .%.[]<- ) x y z = x + y + z;;

  (* the actual issue *)

x.@(4);;
x.@(4) <- 4;;

x.%.{4};;
x.%.{4} <- 4;;

x.%.[4];;
x.%.[4] <- 4;;

(* Constrained unpacks *)

let f = function (module M : S) -> ();;

(* local opens in class and class types *)

class c =
  let open M in
  object end
;;

class type ct =
  let open M in
  object end
;;

(* Docstrings *)

(** Some docstring attached to x. *)
let x =
  42
(** Another docstring attached to x. *)
;;

(* No surrounding parentheses for immediate objects *)
let x = object method f = 1 end;;
let x = object method f = 1 end # f;;
let x = Some object method f = 1 end;;
let x = Some object method f = 1 end # f;;

let f x y z = x in
f object method f = 1 end
  object method f = 1 end # f
  object end
;;

(* Punning of labelled function argument with type constraint *)
let g y =
  let f ~y = y + 1 in
  f ~(y:int)
;;
