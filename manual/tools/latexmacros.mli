type action =
    Print of string
  | Print_arg
  | Skip_arg;;

value find_macro: string -> action list;;

value def_macro: string -> action list -> unit;;
