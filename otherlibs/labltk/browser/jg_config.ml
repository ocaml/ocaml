(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License, with the special exception on linking       *)
(*   described in file ../../../LICENSE.                                 *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

open StdLabels
open Jg_tk

let fixed = if wingui then "{Courier New} 8" else "fixed"
let variable = if wingui then "Arial 9" else "variable"

let init () =
  if wingui then Option.add ~path:"*font" fixed;
  let font =
    let font =
      Option.get Widget.default_toplevel ~name:"variableFont" ~clas:"Font" in
    if font = "" then variable else font
  in
  List.iter ["Button"; "Label"; "Menu"; "Menubutton"; "Radiobutton"]
    ~f:(fun cl -> Option.add ~path:("*" ^ cl ^ ".font") font);
  Option.add ~path:"*Menu.tearOff" "0" ~priority:`StartupFile;
  Option.add ~path:"*Button.padY" "0" ~priority:`StartupFile;
  Option.add ~path:"*Text.highlightThickness" "0" ~priority:`StartupFile;
  Option.add ~path:"*interface.background" "gray85" ~priority:`StartupFile;
  let foreground =
    Option.get Widget.default_toplevel
      ~name:"disabledForeground" ~clas:"Foreground" in
  if foreground = "" then
    Option.add ~path:"*disabledForeground" "black"
