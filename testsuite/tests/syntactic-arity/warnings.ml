(* TEST
expect;
*)

(* Locations of type errors when there is an error
 * partway through checking type parameters. *)

module type S = sig
  type t
  type _ u = t -> t
end

let f (module M : S) = ((fun z -> z) : _ M.u);;
[%%expect {|
module type S = sig type t type _ u = t -> t end
val f : (module M : S) -> 'a M.u = <fun>
|}];;

let f (module M : S) y = ((fun z -> z) : _ M.u);;
[%%expect {|
val f : (module M : S) -> 'a -> 'b M.u = <fun>
|}, Principal{|
val f : (module M : S) -> 'a -> M.t -> M.t = <fun>
|}];;

let f (module M : S) y : _ = ((fun z -> z) : _ M.u);;
[%%expect {|
val f : (module M : S) -> 'a -> 'b M.u = <fun>
|}, Principal{|
val f : (module M : S) -> 'a -> M.t -> M.t = <fun>
|}];;

let f (module M : S) (type a) = ((fun z -> z) : a M.u);;
[%%expect {|
val f : (module M : S) -> 'a M.u = <fun>
|}];;

let f (module M : S) (type a) x = ((fun z -> z) : a M.u);;
[%%expect {|
val f : (module M : S) -> 'b -> 'a M.u = <fun>
|}, Principal{|
val f : (module M : S) -> 'a -> M.t -> M.t = <fun>
|}];;

let f (module M : S) x (type a) = ((fun z -> z) : a M.u);;
[%%expect {|
val f : (module M : S) -> 'b -> 'a M.u = <fun>
|}, Principal{|
val f : (module M : S) -> 'a -> M.t -> M.t = <fun>
|}];;

let f (module M : S) x (type a) :> a M.u = function z -> z
[%%expect {|
val f : (module M : S) -> 'b -> 'a M.u = <fun>
|}, Principal{|
val f : (module M : S) -> 'a -> M.t -> M.t = <fun>
|}];;

let f (module M : S) x (type a) : a M.u = function z -> z
[%%expect {|
val f : (module M : S) -> 'a -> M.t -> M.t = <fun>
|}];;
