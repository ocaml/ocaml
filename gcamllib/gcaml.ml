generic val typeof : {'a} => 'a -> Rtype.type_expr =
  fun ty v -> ty

type dyn = Rtype.type_expr * Obj.t
exception Coercion_failure of Rtype.type_expr * Rtype.type_expr

generic val dyn : {'a} => 'a -> dyn =
  fun ty v -> ty, v

generic val coerce : {'a} => dyn -> 'a =
  fun ty (ty',v) ->
    if ty = ty' (* FIXME *) then Obj.obj v
    else raise (Coercion_failure (ty', ty))

(* Use Gprint.print instead! *)
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

generic val cast : {'a, 'b} => 'a -> 'b =
  fun ta tb v ->
    match ta, tb with
    | [: int :], [: float :] -> Obj.repr (float_of_int (Obj.obj v))
    | [: int :], [: char :] -> Obj.repr (char_of_int (Obj.obj v))
    | [: int :], [: string :] -> Obj.repr (string_of_int (Obj.obj v))
    | [: int :], [: bool :] -> Obj.repr (if Obj.obj v = 0 then false else true)
    | [: float :], [: int :] -> Obj.repr (int_of_float (Obj.obj v))
    | [: float :], [: string :] -> Obj.repr (string_of_float (Obj.obj v))
    | [: char :], [: int :] -> Obj.repr (int_of_char (Obj.obj v))
    | [: char :], [: string :] -> Obj.repr (String.make 1 (Obj.obj v))
    | [: char :], [: bool :] -> Obj.repr (if Obj.obj v = '\000' then false else true)
    | [: string :], [: int :] -> Obj.repr (int_of_string  (Obj.obj v))
    | [: string :], [: float :] -> Obj.repr (float_of_string  (Obj.obj v))
    | [: string :], [: bool :] -> Obj.repr (bool_of_string (Obj.obj v))
    | [: ^x :], [: ^y :] when Rtype.equal x y (* FIXME *) -> v
    | _ -> assert false


