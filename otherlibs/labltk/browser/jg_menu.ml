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

open Tk

class c ~parent ?(underline=0) label = object (self)
  val menu =
    let menu =  Menu.create parent in
    Menu.add_cascade parent ~menu ~label ~underline;
    menu
  method menu = menu
  method virtual add_command :
      ?underline:int ->
      ?accelerator:string ->     ?activebackground:color ->
      ?activeforeground:color -> ?background:color ->
      ?bitmap:bitmap ->          ?command:(unit -> unit) ->
      ?font:string ->            ?foreground:color ->
      ?image:image ->            ?state:state ->
      string -> unit
  method add_command ?(underline=0) ?accelerator ?activebackground
      ?activeforeground ?background ?bitmap ?command ?font ?foreground
      ?image ?state label =
    Menu.add_command menu ~label ~underline ?accelerator
      ?activebackground ?activeforeground ?background ?bitmap
      ?command ?font ?foreground ?image ?state
end

let menubar tl =
  let menu = Menu.create tl ~name:"menubar" ~typ:`Menubar in
  Toplevel.configure tl ~menu;
  menu
