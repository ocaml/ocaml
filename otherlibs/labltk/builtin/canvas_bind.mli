val bind :
  canvas: canvas widget -> events: event list ->
  ?extend: bool -> ?breakable: bool -> ?fields: eventField list ->
  ?action: (eventInfo -> unit) -> tagOrId -> unit 
