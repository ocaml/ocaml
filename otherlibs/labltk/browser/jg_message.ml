(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

open Tk
open Jg_tk

(*
class formatted ~parent ~width ~maxheight ~minheight =
  val parent = (parent : Widget.any Widget.widget)
  val width = width
  val maxheight = maxheight
  val minheight = minheight
  val tw = Text.create ~parent ~width ~wrap:`Word
  val fof = Format.get_formatter_output_functions ()
  method parent = parent
  method init =
    pack [tw] ~side:`Left ~fill:`Both ~expand:true;
    Format.print_flush ();
    Format.set_margin (width - 2);
    Format.set_formatter_output_functions ~out:(Jg_text.output tw)
      ~flush:(fun () -> ())
  method finish =
    Format.print_flush ();
    Format.set_formatter_output_functions ~out:(fst fof) ~flush:(snd fof);
    let `Linechar (l, _) = Text.index tw ~index:(tposend 1) in
    Text.configure tw ~height:(max minheight (min l maxheight));
    if l > 5 then
    pack [Jg_text.add_scrollbar tw] ~before:tw ~side:`Right ~fill:`Y
end
*)

let formatted ~title ?on ?(ppf = Format.std_formatter)
  ?(width=60) ?(maxheight=10) ?(minheight=0) () =
  let tl, frame =
    match on with
      Some frame -> coe frame, frame
    | None ->
        let tl = Jg_toplevel.titled title in
        Jg_bind.escape_destroy tl;
        let frame = Frame.create tl in
        pack [frame] ~side:`Top ~fill:`Both ~expand:true;
        coe tl, frame
  in
  let tw = Text.create frame ~width ~wrap:`Word  in
  pack [tw] ~side:`Left ~fill:`Both ~expand:true;
  Format.pp_print_flush ppf ();
  Format.pp_set_margin ppf (width - 2);
  let fof,fff = Format.pp_get_formatter_output_functions ppf () in
  Format.pp_set_formatter_output_functions ppf
    ~out:(Jg_text.output tw) ~flush:(fun () -> ());
  tl, tw,
  begin fun () ->
    Format.pp_print_flush ppf ();
    Format.pp_set_formatter_output_functions ppf ~out:fof ~flush:fff;
    let `Linechar (l, _) = Text.index tw ~index:(tposend 1) in
    Text.configure tw ~height:(max minheight (min l maxheight));
    if l > 5 then
    pack [Jg_text.add_scrollbar tw] ~before:tw ~side:`Right ~fill:`Y
  end

let ask ~title ?master ?(cancel=true) text =
  let tl = Jg_toplevel.titled title in
  begin match master with None -> ()
  | Some master -> Wm.transient_set tl ~master
  end;
  let mw = Message.create tl ~text ~padx:20 ~pady:10
           ~width:250 ~justify:`Left ~aspect:400 ~anchor:`W
  and fw = Frame.create tl
  and sync = Textvariable.create ~on:tl ()
  and r = ref (`cancel : [`yes|`no|`cancel]) in
  let accept = Button.create fw ~text:"Yes"
      ~command:(fun () -> r := `yes; destroy tl) 
  and refuse = Button.create fw ~text:"No"
      ~command:(fun () -> r := `no; destroy tl)
  and cancelB = Button.create fw ~text:"Cancel"
      ~command:(fun () -> r := `cancel; destroy tl)
  in
  bind tl ~events:[`Destroy] ~extend:true
    ~action:(fun _ -> Textvariable.set sync "1");
  pack [accept; refuse] ~side:`Left ~fill:`X ~expand:true;
  if cancel then pack [cancelB] ~side:`Left ~fill:`X ~expand:true;
  pack [mw] ~side:`Top ~fill:`Both;
  pack [fw] ~side:`Bottom ~fill:`X ~expand:true;
  Grab.set tl;
  Tkwait.variable sync;
  !r
