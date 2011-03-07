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
open Camltk ;;

let top_w = opentk () ;;
let buffer = String.create 256 ;;
let (fd_in, fd_out) = Unix.pipe () ;;
let text0_w = Text.create top_w [] ;;
let entry0_w = Entry.create top_w [] ;;
let button0_w = Button.create top_w [Text "Quit"; Command (fun _ -> exit 0)] ;;
Fileevent.add_fileinput fd_in (fun _ ->
                  let n = Unix.read fd_in buffer 0 (String.length buffer) in
                  let txt = String.sub buffer 0 n in
                  Text.insert text0_w (TextIndex (End, [])) txt []) ;;
let send _ =
 let txt = Entry.get entry0_w ^ "\n" in
 Entry.delete_range entry0_w (At 0) End ;
 ignore (Unix.write fd_out txt 0 (String.length txt));;

bind entry0_w [([], KeyPressDetail "Return")] (BindSet ([], send)) ;
pack [text0_w; entry0_w; button0_w][Side Side_Top; Fill Fill_X; Expand true] ;;
mainLoop () ;;
