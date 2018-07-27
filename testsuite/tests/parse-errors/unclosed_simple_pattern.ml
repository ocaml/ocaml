(* TEST
   * toplevel
*)

let f = function
  | List.(_
;;

let f = function
  | (_
;;

let f = function
  | (_ : int
;;

(* Impossible to get the "unclosed (" message here. This case gets absorbed by
   val_ident... *)

let f = function
  | (module Foo : sig end
;;

(* As with expressions, impossible to get the unclosed message for the following
   cases. *)

let f = function
  | { foo; bar;
;;

let f = function
  | [ 1; 2;
;;

let f = function
  | [| 3; 4;
;;
