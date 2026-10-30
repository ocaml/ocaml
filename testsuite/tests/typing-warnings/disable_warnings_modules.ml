(* TEST
 flags = " -w +A";
 expect;
*)

(* Warning 60 *)

module A = struct
  module type S = sig
    module Foo : sig end
  end
end;;
[%%expect{|
module A : sig module type S = sig module Foo : sig end end end
|}]

module type T = sig
  module G (X : A.S) : sig
    module[@warning "-60"] Bar := X.Foo
  end
end;;

[%%expect {|
module type T = sig module G : (X : A.S) -> sig end end
|}];;


module type T = sig
  module G (X : A.S) : sig
    module Bar := X.Foo [@@warning "-60"]
  end
end;;

[%%expect {|
module type T = sig module G : (X : A.S) -> sig end end
|}];;

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
