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

let version = "$Id$"

(* 
 * convert an integer to an absolute index 
*)
let abs_index n =
  TextIndex (LineChar(0,0), [CharOffset n])

let insertMark =
  TextIndex(Mark "insert", [])

let currentMark =
  TextIndex(Mark "current", [])

let textEnd =
  TextIndex(End, [])

let textBegin =
  TextIndex (LineChar(0,0), [])

(* 
 * Link a scrollbar and a text widget 
*)
let scroll_link sb tx =
  Text.configure tx [YScrollCommand (Scrollbar.set sb)];
  Scrollbar.configure sb [ScrollCommand (Text.yview tx)]


(*
 * Tk 4.0 has navigation in Text widgets, sometimes using scrolling
 * sometimes using the insertion mark. It is a pain to add more
 * compatible bindings. We do our own.
 *)
let page_up tx   =  Text.yview tx (ScrollPage (-1))
and page_down tx =  Text.yview tx (ScrollPage 1)
and line_up tx   =  Text.yview tx (ScrollUnit (-1))
and line_down tx =  Text.yview tx (ScrollUnit 1)
and top tx = Text.yview_index tx textBegin
and bottom tx = Text.yview_index tx textEnd

let navigation_keys tx =
  let tags = bindtags_get tx in
    match tags with
      (WidgetBindings t)::l when t = tx ->
        bindtags tx ((WidgetBindings tx) :: (TagBindings "TEXT_RO") :: l)
    | _ -> ()

let new_scrollable_text top options navigation =
  let f = Frame.create top [] in
  let tx = Text.create f options 
  and sb = Scrollbar.create f [] in
    scroll_link sb tx;
    (* IN THIS ORDER -- RESIZING *)
    pack [sb] [Side Side_Right; Fill Fill_Y];
    pack [tx] [Side Side_Left; Fill Fill_Both; Expand true];
    if navigation then navigation_keys tx;
    f, tx

(*
 * Searching
 *)
let patternv = Frx_misc.autodef Textvariable.create
and casev = Frx_misc.autodef Textvariable.create

let topsearch t =
  (* The user interface *)
  let top = Toplevel.create t [Class "TextSearch"] in
  Wm.title_set top "Text search";
    let f = Frame.create_named top "fpattern" [] in
      let m = Label.create_named f "search" [Text "Search pattern"]
      and e = Entry.create_named f "pattern" 
        [Relief Sunken; TextVariable (patternv()) ] in
  let hgroup = Frame.create top []
  and bgroup = Frame.create top [] in
    let fdir = Frame.create hgroup [] 
    and fmisc = Frame.create hgroup [] in
    let direction = Textvariable.create_temporary fdir
    and exactv = Textvariable.create_temporary fdir
    in
       let forw = Radiobutton.create_named fdir "forward"
             [Text "Forward"; Variable direction; Value "f"]
      and backw = Radiobutton.create_named fdir "backward"
             [Text "Backward"; Variable direction; Value "b"]
      and exact = Checkbutton.create_named fmisc "exact"
             [Text "Exact match"; Variable exactv]
      and case = Checkbutton.create_named fmisc "case"
             [Text "Fold Case"; Variable (casev())] 
      and searchb = Button.create_named bgroup "search" [Text "Search"]
      and contb = Button.create_named bgroup "continue" [Text "Continue"]
      and dismissb = Button.create_named bgroup "dismiss"
         [Text "Dismiss"; 
         Command (fun () -> Text.tag_delete t ["search"]; destroy top)] in

      Radiobutton.invoke forw;
      pack [m][Side Side_Left];
      pack [e][Side Side_Right; Fill Fill_X; Expand true];
      pack [forw; backw] [Anchor W];
      pack [exact; case] [Anchor W];
      pack [fdir; fmisc] [Side Side_Left; Anchor Center];
      pack [searchb; contb; dismissb] [Side Side_Left; Fill Fill_X];
      pack [f;hgroup;bgroup] [Fill Fill_X; Expand true];

  let current_index = ref textBegin in

   let search cont = fun () ->
     let opts = ref [] in
     if Textvariable.get direction = "f" then
        opts := Forwards :: !opts
     else opts := Backwards :: !opts ;
     if Textvariable.get exactv = "1" then
       opts := Exact :: !opts;
     if Textvariable.get (casev()) = "1" then
       opts := Nocase :: !opts;
     try
       let forward = Textvariable.get direction = "f" in
       let i = Text.search t !opts (Entry.get e)
          (if cont then !current_index 
           else if forward then textBegin
           else TextIndex(End, [CharOffset (-1)])) (* does not work with end *)
          (if forward then textEnd 
           else textBegin) in
       let found = TextIndex (i, []) in
         current_index := 
           TextIndex(i, [CharOffset (if forward then 1 else (-1))]);
         Text.tag_delete t ["search"];
         Text.tag_add t "search" found (TextIndex (i, [WordEnd]));
         Text.tag_configure t "search" 
                [Relief Raised; BorderWidth (Pixels 1);
                 Background Red];
         Text.see t found
     with
       Invalid_argument _ -> Bell.ring() in
    
   bind e [[], KeyPressDetail "Return"] 
         (BindSet ([], fun _ -> search false ()));
   Button.configure searchb [Command (search false)];
   Button.configure contb [Command (search true)];
   Tkwait.visibility top;
   Focus.set e

let addsearch tx =
  let tags = bindtags_get tx in
    match tags with
      (WidgetBindings t)::l when t = tx ->
        bindtags tx ((WidgetBindings tx) :: (TagBindings "SEARCH") :: l)
    | _ -> ()

(* We use Mod1 instead of Meta or Alt *)
let init () = 
  List.iter (function ev ->
             tag_bind "TEXT_RO" ev 
                  (BindSetBreakable ([Ev_Widget], 
                                 (fun ei -> page_up ei.ev_Widget; break()))))
           [
            [[], KeyPressDetail "BackSpace"];
            [[], KeyPressDetail "Delete"];
            [[], KeyPressDetail "Prior"];
            [[], KeyPressDetail "b"];
            [[Mod1], KeyPressDetail "v"]
           ];
  List.iter (function ev ->
             tag_bind "TEXT_RO" ev 
                  (BindSetBreakable ([Ev_Widget], 
                                 (fun ei -> page_down ei.ev_Widget; break()))))
           [
            [[], KeyPressDetail "space"];
            [[], KeyPressDetail "Next"];
            [[Control], KeyPressDetail "v"]
           ];
  List.iter (function ev ->
             tag_bind "TEXT_RO" ev 
                  (BindSetBreakable ([Ev_Widget], 
                                 (fun ei -> line_up ei.ev_Widget; break()))))
           [
            [[], KeyPressDetail "Up"];
            [[Mod1], KeyPressDetail "z"]
           ];
  List.iter (function ev ->
             tag_bind "TEXT_RO" ev 
                  (BindSetBreakable ([Ev_Widget], 
                                 (fun ei -> line_down ei.ev_Widget; break()))))
           [
            [[], KeyPressDetail "Down"];
            [[Control], KeyPressDetail "z"]
           ];

  List.iter (function ev ->
             tag_bind "TEXT_RO" ev 
                  (BindSetBreakable ([Ev_Widget], 
                                 (fun ei -> top ei.ev_Widget; break()))))
           [
            [[], KeyPressDetail "Home"];
            [[Mod1], KeyPressDetail "less"]
           ];

  List.iter (function ev ->
             tag_bind "TEXT_RO" ev 
                  (BindSetBreakable ([Ev_Widget], 
                                 (fun ei -> bottom ei.ev_Widget; break()))))
           [
            [[], KeyPressDetail "End"];
            [[Mod1], KeyPressDetail "greater"]
           ];

  List.iter (function ev ->
              tag_bind "SEARCH" ev
                   (BindSetBreakable ([Ev_Widget],
                             (fun ei -> topsearch ei.ev_Widget; break()))))
           [
            [[Control], KeyPressDetail "s"]
           ]

