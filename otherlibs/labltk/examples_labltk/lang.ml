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

(* language encoding using UTF-8 *)
open Tk

let top = opentk () 

(* declare Tk that we use utf-8 to communicate *)
let _ = Encoding.system_set "utf-8"

let b1 = Button.create ~text: "Français" (* french in french *) top 
let b2 = Button.create ~text: "日本語" (* japanese in japanese *) top

let _ = pack [coe b1; coe b2] 

let _ = Printexc.print mainLoop ()
