(* TEST
 expect;
*)

(** This file contains tests relative to transparent signatures (aka dynamic
    aliases, aka present aliases) *)

(** 1. Parsing *)

(** Attributes for dynamic aliases in module expressions and signatures *)
module X0 = struct end
module AttributeItem = X0 [@@dynamic_alias]
module AttributeIdent = X0 [@dynamic_alias]
module[@dynamic_alias] AttributeIdent' = X0
module type DynamicAliasAttributeItem = sig
  module AttributeItem = X0 [@@dynamic_alias]
  module[@dynamic_alias] AttributeIdent = X0
end
[%%expect {|
module X0 : sig end
module AttributeItem = X0 [@@dynamic_alias]
module AttributeIdent = X0 [@@dynamic_alias]
module AttributeIdent' = X0 [@@dynamic_alias]
module type DynamicAliasAttributeItem =
  sig
    module AttributeItem = X0 [@@dynamic_alias]
    module AttributeIdent = X0 [@@dynamic_alias]
  end
|}]


(** 2. Inference *)

(* Strengthening introduces dynamic aliases of module fields *)
module M = struct module X = struct end end
module M' = struct include M end
[%%expect {|
module M : sig module X : sig end end
module M' : sig module X = M.X [@@dynamic_alias]  end
|}]

(* Avoidance should introduce dynamic aliases *)
module X0 = struct end
module M = struct
  open (struct module X1 = X0 [@@dynamic_alias] end)
  module X2 = X1
end
[%%expect{|
module X0 : sig end
module M : sig module X2 = X0 [@@dynamic_alias]  end
|}]

(** 3. Subtyping *)

(* Dynamic aliases are a subtype of static ones *)
module X0 = struct end
module TestSub : sig module X1 = X0 end =
  struct module X1 = X0 [@@dynamic_alias] end
[%%expect{|
module X0 : sig end
module TestSub : sig module X1 = X0 end
|}]

(* Static aliases are *not* a subtype of dynamic ones *)
module X0 = struct end
module TestSub : sig module X1 = X0 [@@dynamic_alias] end =
  struct module X1 = X0 [@@static_alias] end
[%%expect{|
module X0 : sig end
Line 3, characters 2-44:
3 |   struct module X1 = X0 [@@static_alias] end
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig module X1 = X0 end
       is not included in
         sig module X1 = X0 [@@dynamic_alias]  end
       In module "X1":
       Modules do not match:
         (module X0) [@static_alias]
       is not included in
         (module X0) [@dynamic_alias]
|}]

(* Dynamic aliases are a subtype of other dynamic aliases with equivalent
   paths (even with static aliases in the chain of equalities) *)
module X0 = struct end
module X1 = X0 [@dynamic_alias]
module X2 = X0 [@static_alias]
module TestSub_dynamic_alias_chain : sig module X3 = X1 [@@dynamic_alias] end =
struct module X3 = X2 [@dynamic_alias] end
[%%expect{|
module X0 : sig end
module X1 = X0 [@@dynamic_alias]
module X2 = X0
module TestSub_dynamic_alias_chain :
  sig module X3 = X1 [@@dynamic_alias]  end
|}]

(* Dynamic aliasing information can be lost by subtyping *)
module X0 = struct type t = A | B let x = A end
module TestSub_lost_alias_abstract_type :
sig module X1 : sig type t end end =
  struct module X1 = X0 [@dynamic_alias] end
module TestSub_lost_alias_concrete_type :
sig module X1 : sig type t = X0.t = A | B end end =
  struct module X1 = X0 [@dynamic_alias] end
[%%expect{|
module X0 : sig type t = A | B val x : t end
module TestSub_lost_alias_abstract_type : sig module X1 : sig type t end end
module TestSub_lost_alias_concrete_type :
  sig module X1 : sig type t = X0.t = A | B end end
|}]

(* Loosing an alias forces strengthening *)
module X0 = struct type t = A | B let x = A end
module TestSub_functor_call =
  (functor (Y: sig module X1 : sig type t end end) -> Y)(struct
    module X1 = X0 [@dynamic_alias]
  end)
[%%expect{|
module X0 : sig type t = A | B val x : t end
module TestSub_functor_call : sig module X1 : sig type t = X0.t end end
|}]

(* Chain of aliases can be traversed by subtyping *)
module X0 = struct module X = struct type t end end
module X1 = struct module X = X0.X [@@dynamic_alias] end
module Test_Sub_Chain : sig module X : sig type t = X0.X.t end end = X1 [@@dynamic_alias]
[%%expect{|
module X0 : sig module X : sig type t end end
module X1 : sig module X = X0.X [@@dynamic_alias]  end
module Test_Sub_Chain : sig module X : sig type t = X0.X.t end end
|}]


(* Path normalization goes through aliases *)
module X0 = struct end
module XDyn1 = X0 [@@dynamic_alias]
module XDyn2 = X0 [@@dynamic_alias]
module type SDyn1 = sig module X = XDyn1 [@@dynamic_alias] end
module type SDyn2 = sig module X = XDyn2 [@@dynamic_alias] end
let sub_test_dyn : (module SDyn1) -> (module SDyn2) = fun x -> x
module XStat1 = X0 [@@static_alias]
module XStat2 = X0 [@@static_alias]
module type SStat1 = sig module X = XStat1 [@@static_alias] end
module type SStat2 = sig module X = XStat2 [@@static_alias] end
let sub_test_stat : (module SStat1) -> (module SStat2) = fun x -> x
[%%expect{|
module X0 : sig end
module XDyn1 = X0 [@@dynamic_alias]
module XDyn2 = X0 [@@dynamic_alias]
module type SDyn1 = sig module X = XDyn1 [@@dynamic_alias]  end
module type SDyn2 = sig module X = XDyn2 [@@dynamic_alias]  end
val sub_test_dyn : (module SDyn1) -> (module SDyn2) = <fun>
module XStat1 = X0
module XStat2 = X0
module type SStat1 = sig module X = XStat1 end
module type SStat2 = sig module X = XStat2 end
val sub_test_stat : (module SStat1) -> (module SStat2) = <fun>
|}]


(** Invalid attributes throw an error. We test that the [@dynamic_alias]
   attribute throws an error if used:

   1. At a non aliasable positions (outside of a module field of a structure or
   a signature).

   2. With a non-aliasable path (or with something else than a path)

   Similar tests for the [@static_alias] attribute are in [static_aliases.ml]
*)

(* Attribute on a non-aliasable path (functor argument) *)
module X0 = struct end
module F (_:sig end) = struct end
module NonAliasablePath(Y:sig end) = struct
  module X1 = Y [@@dynamic_alias]
end
[%%expect {|
module X0 : sig end
module F : sig end -> sig end
Line 16, characters 14-15:
16 |   module X1 = Y [@@dynamic_alias]
                   ^
Error: Functor arguments and recursive modules (within the
       recursive definition), such as "Y", cannot be aliased
|}]

(* Attribute on a non-aliasable path (recursive module inside the recursive
   knot) *)
module rec X0 : sig end = struct end
and NonAliasablePath : sig end = struct
  module X1 = X0 [@@dynamic_alias]
end
[%%expect {|
Line 3, characters 14-16:
3 |   module X1 = X0 [@@dynamic_alias]
                  ^^
Error: Functor arguments and recursive modules (within the
       recursive definition), such as "X0", cannot be aliased
|}]
