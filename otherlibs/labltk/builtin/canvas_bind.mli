val bind :
  tag: tagOrId -> events: event list ->
  ?extend: bool -> ?breakable: bool -> ?fields: eventField list ->
  ?action: (eventInfo -> unit) -> canvas widget -> unit 
