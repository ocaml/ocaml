(* $Id$ *)

(* Module [Tk]: basic functions and types for LablTk *)

(*** Initialization and termination *)

val openTk : ?display:string -> ?class:string -> unit -> toplevel widget
       (* Initialize LablTk and open a toplevel window.
          [display] is described according to the X11 conventions.
          [class] is used for the X11 resource mechanism. *)
val mainLoop : unit -> unit
       (* Start the main event loop *)
val closeTk : unit -> unit
       (* Quit the main loop and close all open windows. *)
val destroy : 'a Widget.widget -> unit
       (* Destroy an individual widget. *)

(*** Application wide commands *)

val update : unit -> unit
       (* Synchronize display with internal state. *)
val appname_get : unit -> string
val appname_set : string -> unit
       (* Get or set the application name. *)

(*** Widget layout commands *)
type anchor = [`Center|`E|`N|`Ne|`Nw|`S|`Se|`Sw|`W]
type fillMode = [`Both|`None|`X|`Y]
type side = [`Bottom|`Left|`Right|`Top]
val pack :
  ?after:'a Widget.widget ->
  ?anchor:anchor ->
  ?before:'b Widget.widget ->
  ?expand:bool ->
  ?fill:fillMode ->
  ?in:'c Widget.widget ->
  ?ipadx:units ->
  ?ipady:units ->
  ?padx:units ->
  ?pady:units ->
  ?side:side ->
  'd Widget.widget list -> unit
        (* Pack a widget inside its parent,
           using the standard layout engine. *)
val grid :
  ?column:int ->
  ?columnspan:int ->
  ?in:'a Widget.widget ->
  ?ipadx:units ->
  ?ipady:units ->
  ?padx:units ->
  ?pady:units ->
  ?row:int ->
  ?rowspan:int ->
  ?sticky:string -> 'b Widget.widget list -> unit
        (* Pack a widget inside its parent, using the grid layout engine. *)
type borderMode = [`Ignore|`Inside|`Outside]
val place :
  ?anchor:anchor ->
  ?bordermode:borderMode ->
  ?height:units ->
  ?in:'a Widget.widget ->
  ?relheight:float ->
  ?relwidth:float ->
  ?relx:float ->
  ?rely:float ->
  ?width:units ->
  ?x:units -> ?y:units -> 'b Widget.widget -> unit
        (* Pack a widget inside its parent, at absolute coordinates. *)
val raise_window :
  ?above:'a Widget.widget -> 'b Widget.widget -> unit
val lower_window :
  ?below:'a Widget.widget -> 'b Widget.widget -> unit
        (* Raise or lower the window associated to a widget. *)

(*** Event handling *)

type xEvent =
  [`ButtonPress|`ButtonPressDetail int|`ButtonRelease
  |`ButtonReleaseDetail int|`Circulate|`ColorMap
  |`Configure|`Destroy|`Enter|`Expose|`FocusIn|`FocusOut
  |`Gravity|`KeyPress|`KeyPressDetail string|`KeyRelease
  |`KeyReleaseDetail string|`Leave|`Map|`Motion|`Property
  |`Reparent|`Unmap|`Visibility]

type modifier =
  [`Control|`Shift|`Lock|`Button1|`Button2|`Button3
  |`Button4|`Button5|`Double|`Triple|`Mod1|`Mod2
  |`Mod3|`Mod4|`Mod5|`Meta|`Alt]

(* A compound event is a list of events happening in succession,
   each of them possibly qualified by modifiers. That is, compound
   events have type [(modifier list * xEvents) list] *)

type eventInfo =
  { mutable ev_Above: int;
    mutable ev_ButtonNumber: int;
    mutable ev_Count: int;
    mutable ev_Detail: string;
    mutable ev_Focus: bool;
    mutable ev_Height: int;
    mutable ev_KeyCode: int;
    mutable ev_Mode: string;
    mutable ev_OverrideRedirect: bool;
    mutable ev_Place: string;
    mutable ev_State: string;
    mutable ev_Time: int;
    mutable ev_Width: int;
    mutable ev_MouseX: int;
    mutable ev_MouseY: int;
    mutable ev_Char: string;
    mutable ev_BorderWidth: int;
    mutable ev_SendEvent: bool;
    mutable ev_KeySymString: string;
    mutable ev_KeySymInt: int;
    mutable ev_RootWindow: int;
    mutable ev_SubWindow: int;
    mutable ev_Type: int;
    mutable ev_Widget: Widget.any Widget.widget;
    mutable ev_RootX: int;
    mutable ev_RootY: int }

type eventField =
  [`Above|`ButtonNumber|`Count|`Detail|`Focus|`Height
  |`KeyCode|`Mode|`OverrideRedirect|`Place|`State
  |`Time|`Width|`MouseX|`MouseY|`Char|`BorderWidth
  |`SendEvent|`KeySymString|`KeySymInt|`RootWindow
  |`SubWindow|`Type|`Widget|`RootX|`RootY]

type bindAction =
  [`Set eventField list * (eventInfo -> unit)
  |`Setbreakable eventField list * (eventInfo -> unit)
  |`Remove
  |`Extend eventField list * (eventInfo -> unit)]

(* A bound action. [`Set] replaces any existing bound action.
   [`Setbreakable] only differs in that you may use [break] inside
   the action.
   [`Remove] removes all bound actions.
   [`Extend] adds a bound action after the currently defined one. *)

val bind :
  'a Widget.widget ->
  events:(modifier list * xEvent) list -> action:bindAction -> unit
        (* Bind a succession of events on a widget to an action. *)
val class_bind :
  string ->
  events:(modifier list * xEvent) list -> action:bindAction -> unit
        (* Same thing for all widgets of a given class *)
val tag_bind :
  string ->
  events:(modifier list * xEvent) list -> action:bindAction -> unit
        (* Same thing for all widgets having a given tag *)
val break : unit -> unit
        (* Used inside a bound action, do not call other actions
           after this one. This is only possible if this action
           was set with [`Setbreakable] *)
