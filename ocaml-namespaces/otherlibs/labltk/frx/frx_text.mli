(***********************************************************************)
(*                                                                     *)
(*                 MLTk, Tcl/Tk interface of Objective Caml            *)
(*                                                                     *)
(*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    *)
(*               projet Cristal, INRIA Rocquencourt                    *)
(*            Jacques Garrigue, Kyoto University RIMS                  *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique and Kyoto University.  All rights reserved.         *)
(*  This file is distributed under the terms of the GNU Library        *)
(*  General Public License, with the special exception on linking      *)
(*  described in file LICENSE found in the Objective Caml source tree. *)
(*                                                                     *)
(***********************************************************************)
open Camltk

val abs_index : int -> textIndex
  (* [abs_index offs] returns the corresponding TextIndex *)

val insertMark : textIndex
val currentMark : textIndex
val textEnd : textIndex
val textBegin : textIndex
  (* shortcuts for various positions in a text widget *)

val scroll_link : Widget.widget -> Widget.widget -> unit
  (* [scroll_link scrollbar text] links a scrollbar and a text widget
     as expected
   *)

val new_scrollable_text :
  Widget.widget -> options list -> bool -> Widget.widget * Widget.widget
  (* [new_scrollable_text parent opts nav_keys] makes a scrollable text
     widget with optional navigation keys. Returns frame and text widget.
   *)
val addsearch : Widget.widget -> unit
  (* [addsearch textw] adds a search dialog bound on [Control-s]
     on the text widget
   *)

val navigation_keys : Widget.widget -> unit
  (* [navigation_keys textw] adds common navigations functions to [textw] *)

val init : unit -> unit
  (* [init ()] must be called before any of the above features is used *)
