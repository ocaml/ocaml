generic val print : {'a} => Format.formatter -> 'a -> unit =
  fun ty ppf v ->
    let type_expr = Typertype.recover_type_expr ty in
    Toploop.print_value !Toploop.toplevel_env (Obj.repr v) ppf type_expr
