open Widget

(* Events and bindings *)
(* Builtin types *)
(* type *)
type xEvent = [
    `ButtonPress (* also Button, but we omit it *)
  | `ButtonPressDetail (int)
  | `ButtonRelease
  | `ButtonReleaseDetail (int)
  | `Circulate
  | `ColorMap
  | `Configure
  | `Destroy
  | `Enter
  | `Expose
  | `FocusIn
  | `FocusOut
  | `Gravity
  | `KeyPress (* also Key, but we omit it *)
  | `KeyPressDetail (string)      (* /usr/include/X11/keysymdef.h *)
  | `KeyRelease
  | `KeyReleaseDetail (string)
  | `Leave
  | `Map
  | `Motion
  | `Property
  | `Reparent
  | `Unmap
  | `Visibility 
]
(* /type *)

(* type *)
type modifier = [
    `Control
  | `Shift
  | `Lock
  | `Button1
  | `Button2
  | `Button3
  | `Button4
  | `Button5
  | `Double
  | `Triple
  | `Mod1
  | `Mod2
  | `Mod3
  | `Mod4
  | `Mod5
  | `Meta
  | `Alt 
]
(* /type *)

(* Event structure, passed to bounded functions *)

(* type *)
type eventInfo =
  {
  mutable ev_Above : int;               (* tk: %a *)
  mutable ev_ButtonNumber : int;        (* tk: %b *)
  mutable ev_Count : int;               (* tk: %c *)
  mutable ev_Detail : string;           (* tk: %d *)
  mutable ev_Focus : bool;              (* tk: %f *)
  mutable ev_Height : int;              (* tk: %h *)
  mutable ev_KeyCode : int;             (* tk: %k *)
  mutable ev_Mode : string;             (* tk: %m *)
  mutable ev_OverrideRedirect : bool;   (* tk: %o *)
  mutable ev_Place : string;            (* tk: %p *)
  mutable ev_State : string;            (* tk: %s *)
  mutable ev_Time : int;                (* tk: %t *)
  mutable ev_Width : int;               (* tk: %w *)
  mutable ev_MouseX : int;              (* tk: %x *)
  mutable ev_MouseY : int;              (* tk: %y *)
  mutable ev_Char : string;             (* tk: %A *)
  mutable ev_BorderWidth : int;         (* tk: %B *)
  mutable ev_SendEvent : bool;          (* tk: %E *)
  mutable ev_KeySymString : string;     (* tk: %K *)
  mutable ev_KeySymInt : int;           (* tk: %N *)
  mutable ev_RootWindow : int;          (* tk: %R *)
  mutable ev_SubWindow : int;           (* tk: %S *)
  mutable ev_Type : int;                (* tk: %T *)
  mutable ev_Widget : any widget;       (* tk: %W *)
  mutable ev_RootX : int;               (* tk: %X *)
  mutable ev_RootY : int                (* tk: %Y *)
  }
(* /type *)


(* To avoid collision with other constructors (Width, State), 
   use Ev_ prefix *)
(* type *)
type eventField = [
    `Above
  | `ButtonNumber
  | `Count
  | `Detail
  | `Focus
  | `Height
  | `KeyCode
  | `Mode
  | `OverrideRedirect
  | `Place
  | `State
  | `Time 
  | `Width
  | `MouseX
  | `MouseY
  | `Char
  | `BorderWidth
  | `SendEvent
  | `KeySymString
  | `KeySymInt
  | `RootWindow
  | `SubWindow
  | `Type
  | `Widget
  | `RootX
  | `RootY
]
(* /type *)

let filleventInfo ev v = function 
    `Above    -> 	ev.ev_Above <- int_of_string v
  | `ButtonNumber -> 	ev.ev_ButtonNumber <- int_of_string v
  | `Count -> 	        ev.ev_Count <- int_of_string v
  | `Detail ->         	ev.ev_Detail <- v
  | `Focus -> 	        ev.ev_Focus <- v = "1"
  | `Height ->         	ev.ev_Height <- int_of_string v
  | `KeyCode ->         ev.ev_KeyCode <- int_of_string v
  | `Mode -> 		ev.ev_Mode <- v
  | `OverrideRedirect -> ev.ev_OverrideRedirect <- v = "1"
  | `Place -> 	        ev.ev_Place <- v
  | `State -> 	        ev.ev_State <- v
  | `Time -> 		ev.ev_Time <- int_of_string v
  | `Width -> 	        ev.ev_Width <- int_of_string v
  | `MouseX -> 	        ev.ev_MouseX <- int_of_string v
  | `MouseY -> 	        ev.ev_MouseY <- int_of_string v
  | `Char -> 		ev.ev_Char <- v
  | `BorderWidth -> 	ev.ev_BorderWidth <- int_of_string v
  | `SendEvent -> 	ev.ev_SendEvent <- v = "1"
  | `KeySymString -> 	ev.ev_KeySymString <- v
  | `KeySymInt -> 	ev.ev_KeySymInt <- int_of_string v
  | `RootWindow -> 	ev.ev_RootWindow <- int_of_string v
  | `SubWindow -> 	ev.ev_SubWindow <- int_of_string v
  | `Type -> 		ev.ev_Type <- int_of_string v
  | `Widget -> 	        ev.ev_Widget <- cTKtoCAMLwidget v
  | `RootX -> 	        ev.ev_RootX <- int_of_string v
  | `RootY -> 	        ev.ev_RootY <- int_of_string v

let wrapeventInfo f what =
  let ev = {
    ev_Above = 0;
    ev_ButtonNumber = 0;
    ev_Count = 0;
    ev_Detail = "";
    ev_Focus = false;
    ev_Height = 0;
    ev_KeyCode = 0;
    ev_Mode = "";
    ev_OverrideRedirect = false;
    ev_Place = "";
    ev_State = "";
    ev_Time = 0;
    ev_Width = 0;
    ev_MouseX = 0;
    ev_MouseY = 0;
    ev_Char = "";
    ev_BorderWidth = 0;
    ev_SendEvent = false;
    ev_KeySymString = "";
    ev_KeySymInt = 0;
    ev_RootWindow = 0;
    ev_SubWindow = 0;
    ev_Type = 0;
    ev_Widget = forget_type default_toplevel;
    ev_RootX = 0;
    ev_RootY = 0 } in
     function args ->
       let l = ref args in
         List.iter fun:(function field ->
	            match !l with
		      [] -> ()
		    | v::rest -> filleventInfo ev v field; l:=rest)
                 what;
       f ev



let rec writeeventField = function
    [] -> ""
  | field::rest ->
    begin
    match field with
        `Above ->     " %a"
      | `ButtonNumber ->" %b"
      | `Count ->     " %c"
      | `Detail ->    " %d"
      | `Focus ->     " %f"
      | `Height ->    " %h"
      | `KeyCode ->   " %k"
      | `Mode ->      " %m"
      | `OverrideRedirect -> " %o"
      | `Place ->     " %p"
      | `State ->     " %s"
      | `Time ->      " %t"
      | `Width ->     " %w"
      | `MouseX ->    " %x"
      | `MouseY ->    " %y"
      (* Quoting is done by Tk *)
      | `Char ->      " %A"
      | `BorderWidth -> " %B"
      | `SendEvent -> " %E"
      | `KeySymString -> " %K"
      | `KeySymInt -> " %N"
      | `RootWindow ->" %R"
      | `SubWindow -> " %S"
      | `Type ->      " %T"
      | `Widget ->" %W"
      | `RootX ->     " %X"
      | `RootY ->     " %Y"
    end 
    ^ writeeventField rest


(* type *)
type bindAction = [
   `Set ( eventField list *  (eventInfo -> unit))
  | `Setbreakable ( eventField list *  (eventInfo -> unit) )
  | `Remove
  | `Extend ( eventField list *  (eventInfo -> unit))
]
(* /type *)


