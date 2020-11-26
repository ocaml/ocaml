(* TEST
   * expect
*)
module Ext (X : sig type 'a t end) = struct
  type t = T : 'a X.t -> t
end;;

let foo (x : Ext(List).t) =
  match x with
  | T l ->
    let open Ext(Array) in
    T (Array.of_list l);;
[%%expect {|
module Ext :
  functor (X : sig type 'a t end) -> sig type t = T : 'a X.t -> t end
Lines 8-9, characters 4-23:
8 | ....let open Ext(Array) in
9 |     T (Array.of_list l)..
Error: This expression has type t but an expression was expected of type
         Ext(Array).t
       The type constructor t would escape its scope
       t is abstract because no corresponding cmi file was found in path.
|}]
