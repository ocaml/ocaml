(* TEST
 expect;
*)

(** This file contains tests relative to static aliases (aka absent aliases) *)

(** 1. Parsing *)

(** Attributes for static aliases in module expressions and signatures. To test
    that the attribute is indeed taken into account (i.e. not just the fallback
    case of the inference), both attributes are added. Type inference always
    checks first for static aliases, then for dynamic ones (regardless of the
    written order). *)
module X0 = struct end
module AttributeItem   = X0 [@@static_alias]  [@@dynamic_alias]
module AttributeItem'  = X0 [@@dynamic_alias] [@@static_alias]
module AttributeIdent  = X0 [@static_alias]   [@dynamic_alias]
module AttributeIdent' = X0 [@dynamic_alias]  [@static_alias]
module[@static_alias] [@dynamic_alias] AttributeItem'' = X0
module[@dynamic_alias] [@static_alias] AttributeItem''' = X0
module type AttributeItem =
  sig
    module AttributeItem  = X0 [@@static_alias] [@@dynamic_alias]
    module AttributeItem' = X0 [@@dynamic_alias] [@@static_alias]
    module[@static_alias] [@dynamic_alias] AttributeIdent = X0
    module[@dynamic_alias] [@static_alias] AttributeIdent' = X0
  end
[%%expect {|
module X0 : sig end
module AttributeItem = X0
module AttributeItem' = X0
module AttributeIdent = X0
module AttributeIdent' = X0
module AttributeItem'' = X0
module AttributeItem''' = X0
module type AttributeItem =
  sig
    module AttributeItem = X0
    module AttributeItem' = X0
    module AttributeIdent = X0
    module AttributeIdent' = X0
  end
|}]


(* Invalid attributes throw an error *)

(* Attribute on a non-aliasable path (functor argument) *)
module X0 = struct end
module F (_:sig end) = struct end
module NonAliasablePath(Y:sig end) = struct
  module X1 = Y [@@static_alias]
end
[%%expect {|
module X0 : sig end
module F : sig end -> sig end
Line 4, characters 14-15:
4 |   module X1 = Y [@@static_alias]
                  ^
Error: Functor arguments and recursive modules (within the
       recursive definition), such as "Y", cannot be aliased
|}]

(* Attribute on a non-aliasable path (recursive module inside the recursive
   knot) *)
module rec X0 : sig end = struct end
and NonAliasablePath : sig end = struct
  module X1 = X0 [@@static_alias]
end
[%%expect {|
Line 3, characters 14-16:
3 |   module X1 = X0 [@@static_alias]
                  ^^
Error: Functor arguments and recursive modules (within the
       recursive definition), such as "X0", cannot be aliased
|}]

(* Attribute on a non-aliasable path (sig) *)
module NonAliasablePath (Y:sig end) = struct
  module type T = sig
    module X1 = Y [@@static_alias]
  end
end
[%%expect {|
Line 3, characters 4-34:
3 |     module X1 = Y [@@static_alias]
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Functor arguments and recursive modules (within the
       recursive definition), such as "Y", cannot be aliased
|}]
