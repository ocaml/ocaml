(* $Id$ *)

(** Basic functions and types for LablTk *)

open Tk
open Widget

(** {2 Initialization and termination} *)

val openTk : ?display:string -> ?clas:string -> unit -> toplevel widget
       (** Initialize LablTk and open a toplevel window.
          [display] is described according to the X11 conventions.
          [clas] is used for the X11 resource mechanism. *)
val mainLoop : unit -> unit
       (** Start the main event loop *)
val closeTk : unit -> unit
       (** Quit the main loop and close all open windows. *)
val destroy : 'a Widget.widget -> unit
       (** Destroy an individual widget. *)

(** {2 Application wide commands} *)

val update : unit -> unit
       (** Synchronize display with internal state. *)

val appname_get : unit -> string
val appname_set : string -> unit
       (** Get or set the application name. *)

(** {2 Dimensions} *)

type units = [`Pix of int | `Cm of float | `In of float | `Mm of float | `Pt of float]
val pixels : units -> int
       (** Converts various on-screen units to pixels,
          respective to the default display. Available units are
          pixels, centimeters, inches, millimeters and points *)

(** {2 Widget layout commands} *)

type anchor = [`Center|`E|`N|`Ne|`Nw|`S|`Se|`Sw|`W]
type fillMode = [`Both|`None|`X|`Y]
type side = [`Bottom|`Left|`Right|`Top]
val pack :
  ?after:'a Widget.widget ->
  ?anchor:anchor ->
  ?before:'b Widget.widget ->
  ?expand:bool ->
  ?fill:fillMode ->
  ?inside:'c Widget.widget ->
  ?ipadx:int ->
  ?ipady:int ->
  ?padx:int ->
  ?pady:int ->
  ?side:side ->
  'd Widget.widget list -> unit
        (** Pack a widget inside its parent,
           using the standard layout engine. *)
val grid :
  ?column:int ->
  ?columnspan:int ->
  ?inside:'a Widget.widget ->
  ?ipadx:int ->
  ?ipady:int ->
  ?padx:int ->
  ?pady:int ->
  ?row:int ->
  ?rowspan:int ->
  ?sticky:string -> 'b Widget.widget list -> unit
        (** Pack a widget inside its parent, using the grid layout engine. *)

type borderMode = [`Ignore|`Inside|`Outside]
val place :
  ?anchor:anchor ->
  ?bordermode:borderMode ->
  ?height:int ->
  ?inside:'a Widget.widget ->
  ?relheight:float ->
  ?relwidth:float ->
  ?relx:float ->
  ?rely:float ->
  ?width:int ->
  ?x:int -> ?y:int -> 'b Widget.widget -> unit
        (** Pack a widget inside its parent, at absolute coordinates. *)

val raise_window :
  ?above:'a Widget.widget -> 'b Widget.widget -> unit
val lower_window :
  ?below:'a Widget.widget -> 'b Widget.widget -> unit
        (** Raise or lower the window associated to a widget. *)

(** {2 Event handling} *)

type modifier =
  [ `Control | `Shift | `Lock
  | `Button1 | `Button2 | `Button3 | `Button4 | `Button5
  | `Double | `Triple
  | `Mod1 | `Mod2 | `Mod3 | `Mod4 | `Mod5 | `Meta | `Alt ]

type event =
  [ `ButtonPress | `ButtonPressDetail of int
  | `ButtonRelease | `ButtonReleaseDetail of int
  | `Circulate | `ColorMap | `Configure | `Destroy
  | `Enter | `Expose | `FocusIn | `FocusOut | `Gravity
  | `KeyPress | `KeyPressDetail of string
  | `KeyRelease | `KeyReleaseDetail of string
  | `Leave | `Map | `Motion | `Property
  | `Reparent | `Unmap | `Visibility
  | `Modified of modifier list * event ]

(** An event can be either a basic X event, or modified by a
   key or mouse modifier. *)

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

(** Event related information accessible in callbacks. *)

type eventField =
  [ `Above | `ButtonNumber | `Count | `Detail | `Focus | `Height
  | `KeyCode | `Mode | `OverrideRedirect | `Place | `State
  | `Time | `Width | `MouseX | `MouseY | `Char | `BorderWidth
  | `SendEvent | `KeySymString | `KeySymInt | `RootWindow
  | `SubWindow | `Type | `Widget | `RootX | `RootY ]

(** In order to access the above event information, one has to pass
   a list of required event fields to the [bind] function. *)

val bind :
  events:event list ->
  ?extend:bool ->
  ?breakable:bool ->
  ?fields:eventField list ->
  ?action:(eventInfo -> unit) ->
  'a Widget.widget -> unit
        (** Bind a succession of [events] on a widget to an [action].
           If [extend] is true then then binding is added after existing
           ones, otherwise it replaces them.
           [breakable] should be true when [break] is to be called inside
           the action.
           [action] is called with the [fields] required set in
           an [eventInfo] structure. Other fields should not be accessed.
           If [action] is omitted then existing bindings are removed. *)

val bind_class :
  events:event list ->
  ?extend:bool ->
  ?breakable:bool ->
  ?fields:eventField list ->
  ?action:(eventInfo -> unit) ->
  ?on:'a Widget.widget ->
  string -> unit
        (** Same thing for all widgets of a given class. If a widget
           is given with label [~on:], the binding will be removed as
           soon as it is destroyed. *)
val bind_tag :
  events:event list ->
  ?extend:bool ->
  ?breakable:bool ->
  ?fields:eventField list ->
  ?action:(eventInfo -> unit) ->
  ?on:'a Widget.widget ->
  string -> unit
        (** Same thing for all widgets having a given tag *)

val break : unit -> unit
        (** Used inside a bound action, do not call other actions
           after this one. This is only possible if this action
           was bound with [~breakable:true]. *)
