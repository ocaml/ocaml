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
open Tk

let top = opentk ()

let scroll_link sb tx =
  Text.configure tx [YScrollCommand (Scrollbar.set sb)];
  Scrollbar.configure sb [ScrollCommand (Text.yview tx)]

let f = Frame.create top []
let text = Text.create f []
let scrollbar = Scrollbar.create f []

let buffer = ref ""

let kill () =
  buffer := 
     Text.get text (TextIndex (Insert, []))
                   (TextIndex (Insert, [LineEnd]));
     Text.delete text (TextIndex (Insert, []))
                   (TextIndex (Insert, [LineEnd]))
;;

let yank () =
  Text.insert text (TextIndex (Insert, [])) !buffer [] 

let _ = bind text [[Control], KeyPressDetail "y"] (BindSet ([], fun _ ->
  yank () ))
;;
let _ = bind text [[Control], KeyPressDetail "k"] (BindSet ([], fun _ ->
  kill () ))
;;

let _ =
  scroll_link scrollbar text;

  pack [text;f][];
  pack [f][];
  mainLoop ()
;;

