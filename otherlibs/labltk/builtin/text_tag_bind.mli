##ifdef CAMLTK

val tag_bind: 
    widget -> textTag -> (modifier list * xEvent) list -> bindAction -> unit 

##else

val tag_bind :
  tag: string -> events: event list ->
  ?extend: bool -> ?breakable: bool -> ?fields: eventField list ->
  ?action: (eventInfo -> unit) -> text widget -> unit 

##endif
