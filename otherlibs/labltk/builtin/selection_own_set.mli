##ifdef CAMLTK

val own_set : icccm list -> widget -> unit 
(** tk invocation: selection own <icccm list> <widget> *)

##else

val own_set :
    ?command:(unit->unit) -> ?selection:string -> 'a widget -> unit 
(** tk invocation: selection own <icccm list> <widget> *)

##endif
