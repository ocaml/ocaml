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
(* Various dialog boxes *)
val open_simple :
  string ->
  (string -> unit) -> (unit -> 'a) -> Textvariable.textVariable -> unit
 (* [open_simple title action cancelled memory]
    A dialog with a message and an entry field (with memory between
    invocations). Either [action] or [cancelled] is called when the user
    answers to the dialog (with Ok or Cancel)
  *)

val open_simple_synchronous : string -> Textvariable.textVariable -> bool
 (* [open_simple_synchronous title memory]
    A synchronous dialog with a message and an entry field (with 
    memory between invocations). Returns true if the user clicks Ok
    or false if the user clicks Cancel.
  *)
val open_list :
  string -> string list -> (string -> unit) -> (unit -> unit) -> unit
 (* [open_list title elements action cancelled]
    A dialog for selecting from a list of elements. [action] is called
    on each selected element, or [cancelled] is called if the user clicks
    Cancel.
  *)

val open_passwd : string -> string * string
 (* [open_passwd title] pops up a username/password dialog and returns
    (username, password).
  *)
