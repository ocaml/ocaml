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
 * Simple spooling for fileinput callbacks
 *)

let waiting_list = Queue. new()
and waiting = ref 0
and max_open = ref 10
and cur_open = ref 0

let add fd f =
  if !cur_open < !max_open then begin
    incr cur_open;
    add_fileinput fd f
    end
  else begin
    incr waiting;
    Queue.add (fd,f) waiting_list
  end

let remove fd =
  
