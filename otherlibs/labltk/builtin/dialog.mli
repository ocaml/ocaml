val create : 
  parent: 'a widget ->
  title: string ->
  message: string ->
  buttons: string list ->
  ?name: string -> ?bitmap: bitmap -> ?default: int -> unit ->int 
  (* [create title message bitmap default button_names parent] 
     cf. tk_dialog *)
