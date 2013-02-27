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

(* Make interface functions available *)
open Camltk;;

(* Initialisation of the interface. *)
let top = opentk ();;
(* top is now the toplevel widget. *)

(* Widget initialisation *)
let b =
  Button.create top [
    Text "foobar";
    Command
      (function () ->
       print_string "foobar";
       print_newline ();
       flush stdout);
  ]
;;
(* Now button [b] exists but is not yet visible. *)

let q =
  Button.create top [
    Text "quit";
    Command closeTk;
  ]
;;
(* Button [q] also exists but is not yet visible. *)

(* Make b and q visible. *)
pack [b; q] [];;

(* Start user interaction. *)
mainLoop ();;
(* You can also quit this program by deleting its main window. *)
