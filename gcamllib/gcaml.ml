open Rtype
open Rtype.Path

generic val typeof : {'a} => 'a -> Rtype.type_expr =
  fun ty v -> ty

type dyn = Rtype.type_expr * Obj.t

generic val dyn : {'a} => 'a -> dyn =
  fun ty v -> ty, v

let constant_printers =
  [ typeof 1, Obj.repr print_int;
    typeof 1.0, Obj.repr print_float;
    typeof "", Obj.repr print_string;
    typeof 'a', Obj.repr print_char;
  ]

generic val print : {'a} => 'a -> unit =
  fun ty v ->
    try
      let constant_printer = List.assoc ty constant_printers in
      ((Obj.obj constant_printer) (Obj.obj v) : unit)
    with
    | Not_found -> print_string "unprintable"
      
