val tag_bind :
  tag: string -> events: event list ->
  ?extend: bool -> ?breakable: bool -> ?fields: eventField list ->
  ?action: (eventInfo -> unit) -> text widget -> unit 
