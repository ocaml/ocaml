##ifdef CAMLTK

val handle_set : icccm list -> widget -> (int -> int -> unit) -> unit
(** tk invocation: selection handle <icccm list> <widget> <command> *)

##else

val handle_set :
    command: (pos:int -> len:int -> string) -> 
    ?format: string -> ?selection:string -> ?typ: string -> 'a widget -> unit
(** tk invocation: selection handle <icccm list> <widget> <command> *)

##endif
