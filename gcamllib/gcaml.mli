open Rtype

val typeof : {'a} => 'a -> type_expr

type dyn
exception Coercion_failure of type_expr * type_expr

val dyn : {'a} => 'a -> dyn
val coerce : {'a} => dyn -> 'a

val print : {'a} => 'a -> unit
(* use Gprint.print instead! *)

val cast : {'a, 'b} => 'a -> 'b
