##ifdef CAMLTK

val bind : widget -> tagOrId -> 
                    (modifier list * xEvent) list -> bindAction -> unit 

##else

val bind :
  events: event list ->
  ?extend: bool ->
  ?breakable: bool ->
  ?fields: eventField list ->
  ?action: (eventInfo -> unit) ->
  canvas widget -> tagOrId -> unit 

##endif
