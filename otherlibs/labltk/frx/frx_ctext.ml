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
(* A trick by Steve Ball to do pixel scrolling on text widgets *)
(* USES frx_fit *)
open Camltk

let create top opts navigation =
  let f = Frame.create top [BorderWidth (Pixels 2); Relief Raised] in
  let lf = Frame.create f [] in
  let rf = Frame.create f [] in
  let c = Canvas.create lf [BorderWidth (Pixels 0)]
  and xscroll = Scrollbar.create lf [Orient Horizontal]
  and yscroll = Scrollbar.create rf [Orient Vertical] 
  and secret = Frame.create_named rf "secret" []
  in
  let t = Text.create c (BorderWidth(Pixels 0) :: opts) in
    if navigation then Frx_text.navigation_keys t;
 
    (* Make the text widget an embedded canvas object *)
    ignore
     (Canvas.create_window c (Pixels 0) (Pixels 0)
        [Anchor NW; Window t; Tags [Tag "main"]]);
    Canvas.focus c (Tag "main");
    (*
    Canvas.configure c [Width (Pixels (Winfo.reqwidth t));
                        Height(Pixels (Winfo.reqheight t))];
    *)
    Canvas.configure c [YScrollCommand (Scrollbar.set yscroll)];
    (* The horizontal scrollbar is directly attached to the
     * text widget, because h scrolling works properly *)
    Scrollbar.configure xscroll [ScrollCommand (Text.xview t)];
    (* But vertical scroll is attached to the canvas *)
    Scrollbar.configure yscroll [ScrollCommand (Canvas.yview c)];
    let scroll, check = Frx_fit.vert t in
    Text.configure t [
        XScrollCommand (Scrollbar.set xscroll);
        YScrollCommand (fun first last ->
           scroll first last;
           let x,y,w,h = Canvas.bbox c [Tag "main"] in
             Canvas.configure c 
               [ScrollRegion (Pixels x, Pixels y, Pixels w, Pixels h)])
        ];

    bind c [[],Configure] (BindSet ([Ev_Width], (fun ei ->
      Canvas.configure_window c (Tag "main") [Width (Pixels ei.ev_Width)])));

    pack [rf] [Side Side_Right; Fill Fill_Y];
    pack [lf] [Side Side_Left; Fill Fill_Both; Expand true];
    pack [secret] [Side Side_Bottom];
    pack [yscroll] [Side Side_Top; Fill Fill_Y; Expand true];
    pack [xscroll] [Side Side_Bottom; Fill Fill_X];
    pack [c] [Side Side_Left; Fill Fill_Both; Expand true];
    f, t
