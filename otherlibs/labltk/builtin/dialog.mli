##ifdef CAMLTK

val create : ?name: string -> 
  widget -> string -> string -> bitmap -> int -> string list -> int 
  (* [create ~name parent title message bitmap default button_names] 
     cf. tk_dialog *)

val create_named :
  widget -> string -> string -> string -> bitmap -> int -> string list -> int 
  (* [create_named parent name title message bitmap default button_names] 
     cf. tk_dialog *)

##else

val create : 
  parent: 'a widget ->
  title: string ->
  message: string ->
  buttons: string list ->
  ?name: string -> ?bitmap: bitmap -> ?default: int -> unit ->int 
  (* [create title message bitmap default button_names parent] 
     cf. tk_dialog *)

##endif
