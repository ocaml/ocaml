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
val scroll_link : Widget.widget -> Widget.widget -> unit
  (* [scroll_link scrollbar listbox] links [scrollbar] and [listbox]
     as expected.
   *)

val add_completion : Widget.widget -> (eventInfo -> unit) -> unit
  (* [add_completion listbox action] adds Macintosh like electric navigation
     in the listbox when characters are typed in.
     [action] is invoked if Return is pressed
   *)

val new_scrollable_listbox :
  Widget.widget -> options list -> Widget.widget * Widget.widget
  (* [new_scrollable_listbox parent options] makes a scrollable listbox and
     returns (frame, listbox)
   *)
