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

let version = "$Id$"

(*
 * Tk 4.0 has emacs bindings for entry widgets
 *)

let new_label_entry parent txt action =
  let f = Frame.create parent [] in
  let m = Label.create f [Text txt]
  and e = Entry.create f [Relief Sunken; TextWidth 0] in
   Camltk.bind e [[], KeyPressDetail "Return"] 
       (BindSet ([], fun _ -> action(Entry.get e)));
  pack [m][Side Side_Left];
  pack [e][Side Side_Right; Fill Fill_X; Expand true];
  f,e

let new_labelm_entry parent txt memo =
  let f = Frame.create parent [] in
  let m = Label.create f [Text txt]
  and e = Entry.create f [Relief Sunken; TextVariable memo; TextWidth 0] in
  pack [m][Side Side_Left];
  pack [e][Side Side_Right; Fill Fill_X; Expand true];
  f,e


