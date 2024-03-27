(* TEST
  expect;
*)

let id_list (l : 'a list) : 'a List.t = l

let list1 = id_list [3; 5; 6]

[%%expect{|
val id_list : 'a list -> 'a List.t = <fun>
val list1 : int List.t = [3; 5; 6]
|}]

module type Map = sig
  type _ t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module FMap (X : Map) = struct
  type 'a t = 'a X.t
  let map = X.map
end

let list2 : int FMap(List).t = [3; 5]

[%%expect{|
module type Map = sig type _ t val map : ('a -> 'b) -> 'a t -> 'b t end
module FMap :
  functor (X : Map) ->
    sig type 'a t = 'a X.t val map : ('a -> 'b) -> 'a X.t -> 'b X.t end
val list2 : int FMap(List).t = [3; 5]
|}]
