open Rtype
open Rtype.Path

generic val typeof : {'a} => 'a -> type_expr =
  fun ty v -> ty

type dyn = type_expr * Obj.t
exception Coercion_failure of type_expr * type_expr

generic val dyn : {'a} => 'a -> dyn =
  fun ty v -> ty, v

generic val coerce : {'a} => dyn -> 'a =
  fun ty (ty',v) ->
    if ty = ty' (* FIXME *) then Obj.obj v
    else raise (Coercion_failure (ty', ty))

generic val print : {'a} => 'a -> unit =
  let rec f ty v =
    match ty with
    | [: int :] -> print_int (Obj.obj v)
    | [: float :] -> print_float (Obj.obj v)
    | [: string :] -> print_string (Obj.obj v)
    | [: char :] -> print_char (Obj.obj v)
    | [: ^x * ^y :] -> f x (Obj.field v 0); f y (Obj.field v 1)
    | [: ^x -> ^y :] -> print_string "<fun>"
    | _ -> print_string "<unprintable>"
  in
  f
