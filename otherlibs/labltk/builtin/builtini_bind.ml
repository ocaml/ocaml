##ifdef CAMLTK

let cCAMLtoTKxEvent = function
  | Activate -> "Activate"
  | ButtonPress -> "ButtonPress"
  | ButtonPressDetail n -> "ButtonPress-"^string_of_int n
  | ButtonRelease -> "ButtonRelease"
  | ButtonReleaseDetail n -> "ButtonRelease-"^string_of_int n
  | Circulate -> "Circulate"
  | ColorMap -> "Colormap"
  | Configure -> "Configure"
  | Deactivate -> "Deactivate"
  | Destroy -> "Destroy"
  | Enter -> "Enter"
  | Expose -> "Expose"
  | FocusIn -> "FocusIn"
  | FocusOut -> "FocusOut"
  | Gravity -> "Gravity"
  | KeyPress -> "KeyPress"
  | KeyPressDetail s -> "KeyPress-"^s
  | KeyRelease -> "KeyRelease"
  | KeyReleaseDetail s -> "KeyRelease-"^s
  | Leave -> "Leave"
  | Map -> "Map"
  | Motion -> "Motion"
  | Property -> "Property"
  | Reparent -> "Reparent"
  | Unmap -> "Unmap"
  | Visibility -> "Visibility" 
  | Virtual s -> "<"^s^">"
;;

let cCAMLtoTKmodifier = function
  | Control -> "Control-"
  | Shift -> "Shift-"
  | Lock -> "Lock-"
  | Button1 -> "Button1-"
  | Button2 -> "Button2-"
  | Button3 -> "Button3-"
  | Button4 -> "Button4-"
  | Button5 -> "Button5-"
  | Double -> "Double-"
  | Triple -> "Triple-"
  | Mod1 -> "Mod1-"
  | Mod2 -> "Mod2-"
  | Mod3 -> "Mod3-"
  | Mod4 -> "Mod4-"
  | Mod5 -> "Mod5-"
  | Meta -> "Meta-"
  | Alt -> "Alt-"
;;

exception IllegalVirtualEvent

(* type event = modifier list * xEvent *)
let cCAMLtoTKevent (ml, xe) =
  match xe with
  | Virtual s -> 
      if ml = [] then "<<"^s^">>"
      else raise IllegalVirtualEvent
  | _ ->
      "<" ^ (String.concat " " (List.map cCAMLtoTKmodifier ml))  
      ^ (cCAMLtoTKxEvent xe) ^ ">"
;;
  
(* type eventSequence == (modifier list * xEvent) list *)
let cCAMLtoTKeventSequence l =
  TkToken(List.fold_left (^) "" (List.map cCAMLtoTKevent l))

##else

let cCAMLtoTKmodifier : modifier -> string = function
 | `Control -> "Control-"
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
;;

exception IllegalVirtualEvent

let cCAMLtoTKevent (ev : event) =
  let modified = ref false in
  let rec convert = function
    | `Activate -> "Activate"
    | `ButtonPress -> "ButtonPress"
    | `ButtonPressDetail n -> "ButtonPress-"^string_of_int n
    | `ButtonRelease -> "ButtonRelease"
    | `ButtonReleaseDetail n -> "ButtonRelease-"^string_of_int n
    | `Circulate -> "Circulate"
    | `Colormap -> "Colormap"
    | `Configure -> "Configure"
    | `Deactivate -> "Deactivate"
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
    | `Virtual s -> 
        if !modified then raise IllegalVirtualEvent else "<"^s^">"
    | `Modified(ml, ev) ->
        modified := true;
        String.concat ~sep:"" (List.map ~f:cCAMLtoTKmodifier ml)
        ^ convert ev
  in "<" ^ convert ev ^ ">"
;;

let cCAMLtoTKeventSequence (l : event list) = 
  TkToken(String.concat ~sep:"" (List.map ~f:cCAMLtoTKevent l))
;;

##endif
