(* Various indexes
    canvas
    entry
    listbox
*)

type canvas_index = [
    `Num(int)		
  | `End		        
  | `Insert		
  | `Selfirst		
  | `Sellast		
  | `Atxy(int * int)         
]

type entry_index = [
    `Num(int)		
  | `End
  | `Insert		
  | `Selfirst		
  | `Sellast		
  | `At(int)
  | `Anchor
]

type listbox_index = [
    `Num(int)		
  | `Active
  | `Anchor
  | `End
  | `Atxy(int * int)
]

type menu_index = [
    `Num(int)		
  | `Active
  | `End
  | `Last			
  | `None		
  | `At(int)
  | `Pattern(string)
]

type text_index = [
    `Linechar(int * int)   
  | `Atxy(int * int)
  | `End
  | `Mark(string)
  | `Tagfirst(string)
  | `Taglast(string)
  | `Window(any widget)
  | `Image(string) 
]

type linechar_index = int * int
type num_index = int
