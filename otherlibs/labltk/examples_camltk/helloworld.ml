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
open Camltk;;            (* Make interface functions available *)

let top = opentk ();;   (* Initialisation of the interface *)
(* top is now the toplevel widget *)

(* Widget initialisation *)
let b = Button.create top 
          [Text "foobar"; 
           Command (function () -> 
                      print_string "foobar"; 
                      print_newline();
                      flush stdout)];;
(* b exists but is not yet visible *)

let q = Button.create top 
          [Text "quit"; 
           Command closeTk];;
(* q exists but is not yet visible *)

pack [b; q][] ;;           (* Make b visible *)
mainLoop() ;;           (* User interaction*)
(* You can quit this program by deleting its main window *)
