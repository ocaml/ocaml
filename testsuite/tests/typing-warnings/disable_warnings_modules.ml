(* TEST
 flags = " -w +A";
 expect;
*)

(* Warning 67 *)

module type[@warning "-67"] S = functor (Unused : sig end) -> sig end;;

[%%expect{|
module type S = (Unused : sig end) -> sig end
|}]


module type S = functor (Unused : sig end) -> sig end [@@warning "-67"];;

[%%expect{|
module type S = (Unused : sig end) -> sig end
|}]

(* Warning 69 *)

module Unused_record : sig end = struct
  type[@warning "-69"] t = { a : int; b : int }
  let foo (x : t) = x
  let _ = foo
end;;

[%%expect{|
module Unused_record : sig end
|}]


module Unused_record : sig end = struct
  type t = { a : int; b : int } [@@warning "-69"]
  let foo (x : t) = x
  let _ = foo
end;;

[%%expect{|
module Unused_record : sig end
|}]


module Unused_record : sig end = struct
  [@@@warning "-69"]
  type t = { a : int; b : int }
  let foo (x : t) = x
  let _ = foo
end;;

[%%expect{|
module Unused_record : sig end
|}]

(* Warning 73 *)

module type S = sig val x : int end;;
let v = (module struct let x = 3 end : S);;
module F() = (val v);;

module[@warning "-73"] M = F(struct end);;

[%%expect{|
module type S = sig val x : int end
val v : (module S) = <module>
module F : () -> S
module M : S
|}]

module M = F(struct end)[@@warning "-73"];;

[%%expect{|
module M : S
|}]
