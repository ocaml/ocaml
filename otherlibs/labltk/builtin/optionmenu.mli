##ifdef CAMLTK

(* Support for tk_optionMenu *)
val create: ?name: string -> 
  widget -> textVariable -> string list -> widget * widget
(** [create ?name parent var options] creates a multi-option menubutton and 
   its associated menu. The option is also stored in the variable. 
   Both widgets (menubutton and menu) are returned. *)

##else

(* Support for tk_optionMenu *)
val create: 
    parent:'a widget -> 
    variable:textVariable ->
    ?name: string -> string list -> menubutton widget * menu widget
(** [create ~parent ~var ~name options] creates a multi-option menubutton 
   and its associated menu. The option is also stored in the variable. 
   Both widgets (menubutton and menu) are returned *)

##endif
