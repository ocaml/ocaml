let cCAMLtoTKxEvent : xEvent -> string = function
    `ButtonPress -> "ButtonPress"
  | `ButtonPressDetail n -> "ButtonPress-"^string_of_int n
  | `ButtonRelease -> "ButtonRelease"
  | `ButtonReleaseDetail n -> "ButtonRelease-"^string_of_int n
  | `Circulate -> "Circulate"
  | `ColorMap -> "ColorMap"
  | `Configure -> "Configure"
  | `Destroy -> "Destroy"
  | `Enter -> "Enter"
  | `Expose -> "Expose"
  | `FocusIn -> "FocusIn"
  | `FocusOut -> "FocusOut"
  | `Gravity -> "Gravity"
  | `KeyPress -> "KeyPress"
  | `KeyPressDetail s -> "KeyPress-"^s
  | `KeyRelease -> "KeyRelease"
  | `KeyReleaseDetail s -> "KeyRelease-"^s
  | `Leave -> "Leave"
  | `Map -> "Map"
  | `Motion -> "Motion"
  | `Property -> "Property"
  | `Reparent -> "Reparent"
  | `Unmap -> "Unmap"
  | `Visibility -> "Visibility" 

let cCAMLtoTKmodifier : modifier -> string = function
   `Control -> "Control-"
 | `Shift -> "Shift-"
 | `Lock -> "Lock-"
 | `Button1 -> "Button1-"
 | `Button2 -> "Button2-"
 | `Button3 -> "Button3-"
 | `Button4 -> "Button4-"
 | `Button5 -> "Button5-"
 | `Double -> "Double-"
 | `Triple -> "Triple-"
 | `Mod1 -> "Mod1-"
 | `Mod2 -> "Mod2-"
 | `Mod3 -> "Mod3-"
 | `Mod4 -> "Mod4-"
 | `Mod5 -> "Mod5-"
 | `Meta -> "Meta-"
 | `Alt -> "Alt-"


(* type event = modifier list * xEvent *)
let cCAMLtoTKevent : (modifier list * xEvent) -> string = 
 function (ml, xe) ->
  "<" ^ (catenate_sep " " (List.map fun:cCAMLtoTKmodifier ml))  
      ^ (cCAMLtoTKxEvent xe) ^ ">"
  
(* type eventSequence == (modifier list * xEvent) list *)
let cCAMLtoTKeventSequence : (modifier list * xEvent) list -> tkArgs = 
 function l ->
  TkToken(catenate_sep "" (List.map fun:cCAMLtoTKevent l))


