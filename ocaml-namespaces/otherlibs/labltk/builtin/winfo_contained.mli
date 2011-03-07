##ifdef CAMLTK

val contained : int -> int -> widget -> bool
(** [contained x y w] returns true if (x,y) is in w *)

##else

val contained : x:int -> y:int -> 'a widget -> bool
(** [contained x y w] returns true if (x,y) is in w *)

##endif
