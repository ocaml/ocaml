(***********************************************************************)
(*                                                                     *)
(*                 MLTk, Tcl/Tk interface of OCaml                     *)
(*                                                                     *)
(*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    *)
(*               projet Cristal, INRIA Rocquencourt                    *)
(*            Jacques Garrigue, Kyoto University RIMS                  *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique and Kyoto University.  All rights reserved.         *)
(*  This file is distributed under the terms of the GNU Library        *)
(*  General Public License, with the special exception on linking      *)
(*  described in file LICENSE found in the OCaml source tree.          *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Support

(*
 * Widgets
 *)

exception IllegalWidgetType of string
      (* Raised when widget command applied illegally*)

(***************************************************)
(* Widgets *)
(* This 'a raw_widget will be 'a Widget.widget     *)
(***************************************************)
type 'a raw_widget =
  Untyped of string
| Typed of string * string

type raw_any (* will be Widget.any *)
and button
and canvas
and checkbutton
and entry
and frame
and label
and listbox
and menu
and menubutton
and message
and radiobutton
and scale
and scrollbar
and text
and toplevel

let forget_type w = (Obj.magic (w : 'a raw_widget) : raw_any raw_widget)
let coe = forget_type

(* table of widgets *)
let table = (Hashtbl.create 401 : (string, raw_any raw_widget) Hashtbl.t)

let name = function
    Untyped s -> s
 |  Typed (s,_) -> s

(* Normally all widgets are known *)
(* this is a provision for send commands to external tk processes *)
let known_class = function
    Untyped _ -> "unknown"
  | Typed (_,c) -> c

(* This one is always created by opentk *)
let default_toplevel =
  let wname = "." in
  let w = Typed (wname, "toplevel") in
    Hashtbl.add table wname w;
    w

(* Dummy widget to which global callbacks are associated *)
(* also passed around by camltotkoption when no widget in context *)
let dummy =
  Untyped "dummy"

let remove w =
  Hashtbl.remove table (name w)

(* Retype widgets returned from Tk *)
(* JPF report: sometime s is "", see Protocol.cTKtoCAMLwidget *)
let get_atom s =
  try
    Hashtbl.find table s
  with
    Not_found -> Untyped s

let naming_scheme = [
        "button", "b";
        "canvas", "ca";
        "checkbutton", "cb";
        "entry", "en";
        "frame", "f";
        "label", "l";
        "listbox", "li";
        "menu", "me";
        "menubutton", "mb";
        "message", "ms";
        "radiobutton", "rb";
        "scale", "sc";
        "scrollbar", "sb";
        "text", "t";
        "toplevel", "top" ]


let widget_any_table =  List.map fst naming_scheme
(* subtypes *)
let widget_button_table = [ "button" ]
and widget_canvas_table = [ "canvas" ]
and widget_checkbutton_table = [ "checkbutton" ]
and widget_entry_table = [ "entry" ]
and widget_frame_table = [ "frame" ]
and widget_label_table = [ "label" ]
and widget_listbox_table = [ "listbox" ]
and widget_menu_table = [ "menu" ]
and widget_menubutton_table = [ "menubutton" ]
and widget_message_table = [ "message" ]
and widget_radiobutton_table = [ "radiobutton" ]
and widget_scale_table = [ "scale" ]
and widget_scrollbar_table = [ "scrollbar" ]
and widget_text_table = [ "text" ]
and widget_toplevel_table = [ "toplevel" ]

let new_suffix clas n =
  try
    (List.assoc clas naming_scheme) ^ (string_of_int n)
  with
    Not_found -> "w" ^ (string_of_int n)

(* The function called by generic creation *)
let counter = ref 0
let new_atom ~parent ?name:nom clas =
  let parentpath = name parent in
    let path =
      match nom with
        None ->
          incr counter;
          if parentpath = "."
          then "." ^ (new_suffix clas !counter)
          else parentpath ^ "." ^ (new_suffix clas !counter)
      | Some name ->
          if parentpath = "."
          then "." ^ name
          else parentpath ^ "." ^ name
    in
      let w = Typed(path,clas) in
        Hashtbl.add table path w;
        w

(* Just create a path. Only to check existence of widgets *)
(* Use with care *)
let atom ~parent ~name:pathcomp =
  let parentpath = name parent in
  let path =
    if parentpath = "."
    then "." ^ pathcomp
    else parentpath ^ "." ^ pathcomp in
      Untyped path

(* LablTk: Redundant with subtyping of Widget, backward compatibility *)
let check_class w clas =
  match w with
    Untyped _ -> () (* assume run-time check by tk*)
  | Typed(_,c) ->
         if List.mem c clas then ()
         else raise (IllegalWidgetType c)


(* Checking membership of constructor in subtype table *)
let chk_sub errname table c =
  if List.mem c table then ()
  else raise (Invalid_argument errname)
