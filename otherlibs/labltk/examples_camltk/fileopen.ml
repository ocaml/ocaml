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
open Camltk;;

let win = opentk();;

let cvs = Canvas.create win [];;

let t = Label.create cvs [Text "File name"];;

let b =
  Button.create cvs
    [Text "Save";
     Command
       (function _ ->
         let s =
           getSaveFile
             [Title "SAVE FILE TEST";
              DefaultExtension ".foo";
              FileTypes [ { typename= "just test";
                            extensions= [".foo"; ".test"];
                            mactypes= ["FOOO"; "BARR"] } ];
              InitialDir Filename.temp_dir_name;
              InitialFile "hogehoge" ] in
         Label.configure t [Text s])];;

let bb =
  Button.create cvs
    [Text "Open";
     Command
       (function _ ->
          let s = getOpenFile [] in
          Label.configure t [Text s])];;

let q =
  Button.create cvs
    [Text "Quit";
     Command
       (function _ -> closeTk (); exit 0)];;

pack [cvs; q;  bb; b; t] [];;

mainLoop ();;
