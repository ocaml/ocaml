generic val typeof : {'a} => 'a -> Rtype.type_expr =
  fun ty v -> ty

type dyn = Rtype.type_expr * Obj.t

generic val dyn : {'a} => 'a -> dyn =
  fun ty v -> ty, v
