(* $Id$ *)

class c :
  parent:'a Widget.widget -> cols:int ->
  texts:string list -> ?maxheight:int -> ?width:int -> unit ->
object
  method cols : int
  method texts : string list
  method parent : Widget.any Widget.widget
  method boxes : Widget.listbox Widget.widget list
  method current : int
  method init : unit
  method recenter : ?aligntop:bool -> int -> unit
  method bind_mouse :
    events:(Tk.modifier list * Tk.xEvent) list ->
    action:(Tk.eventInfo -> index:int -> unit) -> unit
  method bind_kbd :
    events:(Tk.modifier list * Tk.xEvent) list ->
    action:(Tk.eventInfo -> index:int -> unit) -> unit
end

val add_scrollbar : c -> Widget.scrollbar Widget.widget
val add_completion : ?action:(int -> unit) -> ?wait:int -> c -> unit
