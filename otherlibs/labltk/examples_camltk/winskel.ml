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
(* This examples is based on Ousterhout's book (fig 16.15) *)
open Camltk

let main () =
 let top = opentk() in
 let mbar = Frame.create top [Relief Raised; BorderWidth (Pixels 2)] 
 and dummy = 
    Frame.create top [Width (Centimeters 10.); Height (Centimeters 5.)] in
    pack [mbar; dummy] [Side Side_Top; Fill Fill_X];
 let file = Menubutton.create mbar [Text "File"; UnderlinedChar 0]
 and edit = Menubutton.create mbar [Text "Edit"; UnderlinedChar 0]
 and graphics = Menubutton.create mbar [Text "Graphics"; UnderlinedChar 0]
 and text = Menubutton.create mbar [Text "Text"; UnderlinedChar 0]
 and view = Menubutton.create mbar [Text "View"; UnderlinedChar 0]
 and help = Menubutton.create mbar [Text "Help"; UnderlinedChar 0] in
   pack [file;edit;graphics;text;view] [Side Side_Left];
   pack [help] [Side Side_Right];
   (* same code as chap16-14 *)
  let m = Menu.create text [] in
   let bold = Textvariable.create() 
   and italic = Textvariable.create() 
   and underline = Textvariable.create() in
   Menu.add_checkbutton m [Label "Bold"; Variable bold];
   Menu.add_checkbutton m [Label "Italic"; Variable italic];
   Menu.add_checkbutton m [Label "Underline"; Variable underline];
   Menu.add_separator m;
   let font = Textvariable.create() in
   Menu.add_radiobutton m [Label "Times"; Variable font; Value "times"];
   Menu.add_radiobutton m [Label "Helvetica"; Variable font; Value "helvetica"]
;
   Menu.add_radiobutton m [Label "Courier"; Variable font; Value "courier"];
   Menu.add_separator m;
   Menu.add_command m [Label "Insert Bullet";
                        Command (function () -> 
                                  print_string "Insert Bullet\n"; 
                                  flush stdout)];   
   Menu.add_command m [Label "Margins and Tags...";
                        Command (function () -> 
                                  print_string "margins\n"; 
                                  flush stdout)]; 
   Menubutton.configure text [Menu m];

    mainLoop()



let _ =
 Printexc.catch main ()
