##ifdef CAMLTK

open Widget;;

(* Events and bindings *)
(* Builtin types *)
(* type *)
type xEvent =
  | Activate
  | ButtonPress (* also Button, but we omit it *)
  | ButtonPressDetail of int
  | ButtonRelease
  | ButtonReleaseDetail of int
  | Circulate
  | ColorMap (* not Colormap, avoiding confusion between the Colormap option *)
  | Configure
  | Deactivate
  | Destroy
  | Enter
  | Expose
  | FocusIn
  | FocusOut
  | Gravity
  | KeyPress (* also Key, but we omit it *)
  | KeyPressDetail of string      (* /usr/include/X11/keysymdef.h *)
  | KeyRelease
  | KeyReleaseDetail of string
  | Leave
  | Map
  | Motion
  | Property
  | Reparent
  | Unmap
  | Visibility 
  | Virtual of string (* Virtual event. Must be without modifiers *)
;;
(* /type *)

(* type *)
type modifier =
  | Control
  | Shift
  | Lock
  | Button1
  | Button2
  | Button3
  | Button4
  | Button5
  | Double
  | Triple
  | Mod1
  | Mod2
  | Mod3
  | Mod4
  | Mod5
  | Meta
  | Alt 
;;
(* /type *)

(* Event structure, passed to bounded functions *)

(* type *)
type eventInfo =
  {
  (* %# : event serial number is unsupported *)
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
  mutable ev_Widget : widget;           (* tk: %W *)
  mutable ev_RootX : int;               (* tk: %X *)
  mutable ev_RootY : int                (* tk: %Y *)
  }
;;
(* /type *)


(* To avoid collision with other constructors (Width, State), 
   use Ev_ prefix *)
(* type *)
type eventField =
  | Ev_Above
  | Ev_ButtonNumber
  | Ev_Count
  | Ev_Detail
  | Ev_Focus
  | Ev_Height
  | Ev_KeyCode
  | Ev_Mode
  | Ev_OverrideRedirect
  | Ev_Place
  | Ev_State
  | Ev_Time 
  | Ev_Width
  | Ev_MouseX
  | Ev_MouseY
  | Ev_Char
  | Ev_BorderWidth
  | Ev_SendEvent
  | Ev_KeySymString
  | Ev_KeySymInt
  | Ev_RootWindow
  | Ev_SubWindow
  | Ev_Type
  | Ev_Widget
  | Ev_RootX
  | Ev_RootY
;;
(* /type *)

let filleventInfo ev v = function 
  | Ev_Above    ->      ev.ev_Above <- int_of_string v
  | Ev_ButtonNumber ->  ev.ev_ButtonNumber <- int_of_string v
  | Ev_Count ->         ev.ev_Count <- int_of_string v
  | Ev_Detail ->        ev.ev_Detail <- v
  | Ev_Focus ->         ev.ev_Focus <- v = "1"
  | Ev_Height ->        ev.ev_Height <- int_of_string v
  | Ev_KeyCode ->       ev.ev_KeyCode <- int_of_string v
  | Ev_Mode ->          ev.ev_Mode <- v
  | Ev_OverrideRedirect -> ev.ev_OverrideRedirect <- v = "1"
  | Ev_Place ->         ev.ev_Place <- v
  | Ev_State ->         ev.ev_State <- v
  | Ev_Time ->          ev.ev_Time <- int_of_string v
  | Ev_Width ->         ev.ev_Width <- int_of_string v
  | Ev_MouseX ->        ev.ev_MouseX <- int_of_string v
  | Ev_MouseY ->        ev.ev_MouseY <- int_of_string v
  | Ev_Char ->          ev.ev_Char <- v
  | Ev_BorderWidth ->   ev.ev_BorderWidth <- int_of_string v
  | Ev_SendEvent ->     ev.ev_SendEvent <- v = "1"
  | Ev_KeySymString ->  ev.ev_KeySymString <- v
  | Ev_KeySymInt ->     ev.ev_KeySymInt <- int_of_string v
  | Ev_RootWindow ->    ev.ev_RootWindow <- int_of_string v
  | Ev_SubWindow ->     ev.ev_SubWindow <- int_of_string v
  | Ev_Type ->          ev.ev_Type <- int_of_string v
  | Ev_Widget ->        ev.ev_Widget <- cTKtoCAMLwidget v
  | Ev_RootX ->         ev.ev_RootX <- int_of_string v
  | Ev_RootY ->         ev.ev_RootY <- int_of_string v
;;

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
    ev_Widget = Widget.default_toplevel;
    ev_RootX = 0;
    ev_RootY = 0 } in
  function args ->
    let l = ref args in
    List.iter (function field ->
      match !l with
        [] -> ()
      | v::rest -> filleventInfo ev v field; l:=rest)
      what;
    f ev
;;

let rec writeeventField = function
  | [] -> ""
  | field::rest ->
    begin
    match field with
    | Ev_Above ->     " %a"
    | Ev_ButtonNumber ->" %b"
    | Ev_Count ->     " %c"
    | Ev_Detail ->    " %d"
    | Ev_Focus ->     " %f"
    | Ev_Height ->    " %h"
    | Ev_KeyCode ->   " %k"
    | Ev_Mode ->      " %m"
    | Ev_OverrideRedirect -> " %o"
    | Ev_Place ->     " %p"
    | Ev_State ->     " %s"
    | Ev_Time ->      " %t"
    | Ev_Width ->     " %w"
    | Ev_MouseX ->    " %x"
    | Ev_MouseY ->    " %y"
    (* Quoting is done by Tk *)
    | Ev_Char ->      " %A"
    | Ev_BorderWidth -> " %B"
    | Ev_SendEvent -> " %E"
    | Ev_KeySymString -> " %K"
    | Ev_KeySymInt -> " %N"
    | Ev_RootWindow ->" %R"
    | Ev_SubWindow -> " %S"
    | Ev_Type ->      " %T"
    | Ev_Widget ->" %W"
    | Ev_RootX ->     " %X"
    | Ev_RootY ->     " %Y"
    end 
    ^ writeeventField rest
;;

##else

open Widget;;

(* Events and bindings *)
(* Builtin types *)

(* type *)
type event = [
  | `Activate
  | `ButtonPress (* also Button, but we omit it *)
  | `ButtonPressDetail of int
  | `ButtonRelease
  | `ButtonReleaseDetail of int
  | `Circulate
  | `Colormap
  | `Configure
  | `Deactivate
  | `Destroy
  | `Enter
  | `Expose
  | `FocusIn
  | `FocusOut
  | `Gravity
  | `KeyPress (* also Key, but we omit it *)
  | `KeyPressDetail of string      (* /usr/include/X11/keysymdef.h *)
  | `KeyRelease
  | `KeyReleaseDetail of string
  | `Leave
  | `Map
  | `Motion
  | `Property
  | `Reparent
  | `Unmap
  | `Visibility
  | `Virtual of string  (* Virtual event. Must be without modifiers *)
  | `Modified of modifier list * event
]

and modifier = [
  | `Control
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
;;
(* /type *)

(* Event structure, passed to bounded functions *)

(* type *)
type eventInfo = {
  (* %# : event serial number is unsupported *)
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
;;
(* /type *)


(* To avoid collision with other constructors (Width, State), 
   use Ev_ prefix *)
(* type *)
type eventField = [
  | `Above
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
;;
(* /type *)

let filleventInfo ev v : eventField -> unit = function 
  | `Above    ->        ev.ev_Above <- int_of_string v
  | `ButtonNumber ->    ev.ev_ButtonNumber <- int_of_string v
  | `Count ->           ev.ev_Count <- int_of_string v
  | `Detail ->          ev.ev_Detail <- v
  | `Focus ->           ev.ev_Focus <- v = "1"
  | `Height ->          ev.ev_Height <- int_of_string v
  | `KeyCode ->         ev.ev_KeyCode <- int_of_string v
  | `Mode ->            ev.ev_Mode <- v
  | `OverrideRedirect -> ev.ev_OverrideRedirect <- v = "1"
  | `Place ->           ev.ev_Place <- v
  | `State ->           ev.ev_State <- v
  | `Time ->            ev.ev_Time <- int_of_string v
  | `Width ->           ev.ev_Width <- int_of_string v
  | `MouseX ->          ev.ev_MouseX <- int_of_string v
  | `MouseY ->          ev.ev_MouseY <- int_of_string v
  | `Char ->            ev.ev_Char <- v
  | `BorderWidth ->     ev.ev_BorderWidth <- int_of_string v
  | `SendEvent ->       ev.ev_SendEvent <- v = "1"
  | `KeySymString ->    ev.ev_KeySymString <- v
  | `KeySymInt ->       ev.ev_KeySymInt <- int_of_string v
  | `RootWindow ->      ev.ev_RootWindow <- int_of_string v
  | `SubWindow ->       ev.ev_SubWindow <- int_of_string v
  | `Type ->            ev.ev_Type <- int_of_string v
  | `Widget ->          ev.ev_Widget <- cTKtoCAMLwidget v
  | `RootX ->           ev.ev_RootX <- int_of_string v
  | `RootY ->           ev.ev_RootY <- int_of_string v
;;

let wrapeventInfo f (what : eventField list) =
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
       List.iter what ~f:
         begin fun field ->
           match !l with
           | [] -> ()
           | v :: rest -> filleventInfo ev v field; l := rest
         end;
       f ev
;;

let rec writeeventField : eventField list -> string = function
  | [] -> ""
  | field :: rest ->
    begin
    match field with
    | `Above ->     " %a"
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
    | `Widget ->    " %W"
    | `RootX ->     " %X"
    | `RootY ->     " %Y"
    end 
    ^ writeeventField rest
;;

##endif
