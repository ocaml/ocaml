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

let top = opentk ()

let scroll_link sb tx =
  Text.configure tx [YScrollCommand (Scrollbar.set sb)];
  Scrollbar.configure sb [ScrollCommand (Text.yview tx)]

let f = Frame.create top []
let text = Text.create f []
let scrollbar = Scrollbar.create f []

(* kill buffer *)
let buffer = ref ""

(* Note: for the text widgets, the insertion cursor is 
    not TextIndex(Insert, []),
    but TextIndex(Mark  "insert", []) 
*) 
let insertMark = TextIndex(Mark "insert", [])
let eol_insertMark = TextIndex(Mark "insert", [LineEnd])

let kill () =
  buffer := 
     Text.get text insertMark eol_insertMark;
  prerr_endline ("Killed: " ^ !buffer);
  Text.delete text insertMark eol_insertMark
;;

let yank () =
  Text.insert text insertMark !buffer [];
  prerr_endline ("Yanked: " ^ !buffer)
;;

let _ =
  scroll_link scrollbar text;

  pack [text; scrollbar][Side Side_Left; Fill Fill_Y];
  pack [f][];

  bind text [[Control], KeyPressDetail "y"]
   (BindSet ([], fun _ -> yank () ));
  bind text [[Control], KeyPressDetail "k"]
   (BindSet ([], fun _ -> kill () ));

  mainLoop ()
;;

