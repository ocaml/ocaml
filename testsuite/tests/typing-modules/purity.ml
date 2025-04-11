(* TEST
  expect;
*)

module type T = sig module type S end

module type PureF = functor (T : T) => sig module M : T.S end

module type ImpureF = functor (T : T) -> sig module M : T.S end

module type PureFbis = functor [@pure] (T : T) -> sig module M : T.S end

[%%expect{|
module type T = sig module type S end
module type PureF = (T : T) => sig module M : T.S end
module type ImpureF = (T : T) -> sig module M : T.S end
module type PureFbis = (T : T) => sig module M : T.S end
|}]

module PuretoImpure (F : PureF) : sig
  module F : ImpureF
end = struct
  module F = F
end

[%%expect{|
module PuretoImpure : (F : PureF) => sig module F : ImpureF end
|}]

module ImpureToPure_Fail (F : ImpureF) : sig
  module F : PureF
end = struct
  module F = F
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   module F = F
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig module F : (T : T) -> sig module M : T.S end end
       is not included in
         sig module F : PureF end
       In module "F":
       Modules do not match: (T : T) -> ... is not included in (T : T) => ...
       The functor was expected to be pure at this position
|}]

module M (_ : T) = struct end
module Mimpure = functor (_ : T) -> struct exception E end
module Mpure = functor (_ : T) => struct end
module M4 = functor (_ : T) (_ : T) -> struct exception E end
module M5 = functor (_ : T) (_ : T) => struct end
module M6 = functor [@pure] (_ : T) (_ : T) -> struct end

[%%expect{|
module M : T => sig end
module Mimpure : T -> sig exception E end
module Mpure : T => sig end
module M4 : T => T -> sig exception E end
module M5 : T => T => sig end
module M6 : T => T => sig end
|}]

type ext_type = ..
type ext_type += C1

(* Check that all those operations are allowed in a pure functor *)

module F = functor (_ : sig end) => struct
  (fun x -> x);;
  let id x = x

  type t = Constructor
  type ext_type2 = ..

  type ext_type += C1Rebind = C1

  exception F = Not_found

  module M = struct module type S = sig end end
  module M2 = Mpure(M)

  include M2

end

[%%expect{|
type ext_type = ..
type ext_type += C1
module F :
  sig end =>
    sig
      val id : 'a -> 'a
      type t = Constructor
      type ext_type2 = ..
      type ext_type += C1Rebind
      exception F
      module M : sig module type S = sig end end
      module M2 : sig end
    end
|}]

(* Check that all those operations are not allowed in an pure functor *)

module F_fail_expr = functor (_ : sig end) => struct
  print_int 3
end

[%%expect{|
Line 2, characters 2-13:
2 |   print_int 3
      ^^^^^^^^^^^
Error: This expression is not garanted to be pure.
       It is not allowed inside pure applicative functors.
|}]

module F_fail_bind = functor (_ : sig end) => struct
  let () = print_int 3
end

[%%expect{|
Line 2, characters 2-22:
2 |   let () = print_int 3
      ^^^^^^^^^^^^^^^^^^^^
Error: This expression is not garanted to be pure.
       It is not allowed inside pure applicative functors.
|}]

module F_fail_ext = functor (_ : sig end) => struct
  type ext_type += C2
end

[%%expect{|
Line 2, characters 2-21:
2 |   type ext_type += C2
      ^^^^^^^^^^^^^^^^^^^
Error: This expression is not garanted to be pure.
       It is not allowed inside pure applicative functors.
|}]

module F_fail_exception = functor (_ : sig end) => struct
  exception Exc
end

[%%expect{|
Line 2, characters 2-15:
2 |   exception Exc
      ^^^^^^^^^^^^^
Error: This expression is not garanted to be pure.
       It is not allowed inside pure applicative functors.
|}]

module F_fail_impure_app1 = functor (_ : sig end) => struct
  module M = struct module type S = sig end end
  module M2 = Mimpure(M)
end

[%%expect{|
Line 3, characters 14-24:
3 |   module M2 = Mimpure(M)
                  ^^^^^^^^^^
Error: This expression is not garanted to be pure.
       It is not allowed inside pure applicative functors.
|}]

module F_fail_impure_app2 = functor (_ : sig end) => struct
  module G () = struct end
  module M = G()
end

[%%expect{|
Line 3, characters 13-16:
3 |   module M = G()
                 ^^^
Error: This expression is not garanted to be pure.
       It is not allowed inside pure applicative functors.
|}]

module F_fail_class = functor (_ : sig end) => struct
  class c = object
  end
end

[%%expect{|
Lines 2-3, characters 2-5:
2 | ..class c = object
3 |   end
Error: This expression is not garanted to be pure.
       It is not allowed inside pure applicative functors.
|}]

(* Test if purity check works *)

module ShouldBePure (T : sig type t end) = struct
end

[%%expect{|
module ShouldBePure : (T : sig type t end) => sig end
|}]
