(* TEST
   flags = " -w A "
   * expect
*)

module Foo(Unused : sig end) = struct end;;
[%%expect {|
module Foo : functor (Unused : sig  end) -> sig  end
|}]

module type S = functor (Unused : sig end) -> sig end;;
[%%expect {|
module type S = functor (Unused : sig  end) -> sig  end
|}]

module type S = sig
  module M (Unused : sig end) : sig end
end;;
[%%expect{|
module type S = sig module M : functor (Unused : sig  end) -> sig  end end
|}]
