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

(* Temporary focus *)

(* ? use bind tag ? how about the global reference then *)
let auto w =
  let old_focus = ref w in
  bind w [[],Enter] 
      (BindSet([], fun _ -> old_focus := Focus.get (); Focus.set w));
  bind w [[],Leave] 
      (BindSet([], fun _ -> Focus.set !old_focus))
