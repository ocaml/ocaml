(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License, with the special exception on linking       *)
(*   described in file ../../../LICENSE.                                 *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

class c :
  cols:int -> texts:string list ->
  ?maxheight:int -> ?width:int -> 'a Widget.widget ->
object
  method cols : int
  method texts : string list
  method parent : Widget.any Widget.widget
  method boxes : Widget.listbox Widget.widget list
  method current : int
  method init : unit
  method recenter : ?aligntop:bool -> int -> unit
  method bind_mouse :
    events:Tk.event list -> action:(Tk.eventInfo -> index:int -> unit) -> unit
  method bind_kbd :
    events:Tk.event list -> action:(Tk.eventInfo -> index:int -> unit) -> unit
end

val add_scrollbar : c -> Widget.scrollbar Widget.widget
val add_completion : ?action:(int -> unit) -> ?wait:int -> c -> unit
