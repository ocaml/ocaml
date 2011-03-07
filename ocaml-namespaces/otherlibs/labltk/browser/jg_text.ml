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
open Tk
open Jg_tk

let get_all tw = Text.get tw ~start:tstart ~stop:(tposend 1)

let tag_and_see tw ~tag ~start ~stop =
  Text.tag_remove tw ~start:(tpos 0) ~stop:tend ~tag;
  Text.tag_add tw ~start ~stop ~tag;
  try
    Text.see tw ~index:(`Tagfirst tag, []);
    Text.mark_set tw ~mark:"insert" ~index:(`Tagfirst tag, [])
  with Protocol.TkError _ -> ()

let output tw ~buf ~pos ~len =
  Text.insert tw ~index:tend ~text:(String.sub buf ~pos ~len)

let add_scrollbar tw =
  let sb = Scrollbar.create (Winfo.parent tw) ~command:(Text.yview tw)
  in Text.configure tw ~yscrollcommand:(Scrollbar.set sb); sb

let create_with_scrollbar parent =
  let frame = Frame.create parent in
  let tw = Text.create frame in
  frame, tw, add_scrollbar tw

let goto_tag tw ~tag =
  let index = (`Tagfirst tag, []) in
  try Text.see tw ~index;
      Text.mark_set tw ~index ~mark:"insert"
  with Protocol.TkError _ -> ()

let search_string tw =
  let tl = Jg_toplevel.titled "Search" in
  Wm.transient_set tl ~master:(Winfo.toplevel tw);
  let fi = Frame.create tl
  and fd = Frame.create tl
  and fm = Frame.create tl
  and buttons = Frame.create tl
  and direction = Textvariable.create ~on:tl ()
  and mode = Textvariable.create ~on:tl ()
  and count = Textvariable.create ~on:tl ()
  in
  let label = Label.create fi ~text:"Pattern:"
  and text = Entry.create fi ~width:20
  and back = Radiobutton.create fd ~variable:direction
               ~text:"Backwards" ~value:"backward"
  and forw = Radiobutton.create fd ~variable:direction
               ~text:"Forwards" ~value:"forward"
  and exact = Radiobutton.create fm ~variable:mode
                ~text:"Exact" ~value:"exact"
  and nocase = Radiobutton.create fm ~variable:mode
                 ~text:"No case" ~value:"nocase"
  and regexp =  Radiobutton.create fm ~variable:mode
                 ~text:"Regexp" ~value:"regexp"
  in
  let search = Button.create buttons ~text:"Search" ~command:
    begin fun () ->
    try
      let pattern = Entry.get text in
      let dir, ofs = match Textvariable.get direction with
          "forward" -> `Forwards, 1
        | "backward" -> `Backwards, -1
        | _ -> assert false
      and mode = match Textvariable.get mode with "exact" -> [`Exact]
                 | "nocase" -> [`Nocase] | "regexp" -> [`Regexp] | _ -> []
      in
      let ndx =
        Text.search tw ~pattern ~switches:([dir;`Count count] @ mode)
          ~start:(`Mark "insert", [`Char ofs])
      in
      tag_and_see tw ~tag:"sel" ~start:(ndx,[])
        ~stop:(ndx,[`Char(int_of_string (Textvariable.get count))])
    with Invalid_argument _ -> ()
    end
  and ok = Jg_button.create_destroyer tl ~parent:buttons ~text:"Cancel" in

  Focus.set text;
  Jg_bind.return_invoke text ~button:search;
  Jg_bind.escape_destroy tl;
  Textvariable.set direction "forward";
  Textvariable.set mode "nocase";
  pack [label] ~side:`Left;
  pack [text] ~side:`Right ~fill:`X ~expand:true;
  pack [back; forw] ~side:`Left;
  pack [exact; nocase; regexp] ~side:`Left;
  pack [search; ok] ~side:`Left ~fill:`X ~expand:true;
  pack [fi; fd; fm; buttons] ~side:`Top ~fill:`X
