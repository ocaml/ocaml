(* TEST
 expect;
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
module Ext : (X : sig type 'a t end) -> sig type t = T : 'a X.t -> t end
val foo : Ext(List).t -> Ext(Array).t = <fun>
|}]
