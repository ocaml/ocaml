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

let add_scrollbar lb  =
  let sb =
    Scrollbar.create (Winfo.parent lb) ~command:(Listbox.yview lb) in
  Listbox.configure lb ~yscrollcommand:(Scrollbar.set sb); sb

let create_with_scrollbar ?selectmode parent =
  let frame = Frame.create parent in
  let lb = Listbox.create frame ?selectmode in
  frame, lb, add_scrollbar lb

(* from frx_listbox,adapted *)

let recenter lb ~index =
   Listbox.selection_clear lb ~first:(`Num 0) ~last:`End;
     (* Activate it, to keep consistent with Up/Down.
        You have to be in Extended or Browse mode *)
   Listbox.activate lb ~index;
   Listbox.selection_anchor lb ~index;
   Listbox.yview_index lb ~index

class timed ?wait ?nocase get_texts = object
  val get_texts = get_texts
  inherit Jg_completion.timed [] ?wait ?nocase as super
  method! reset =
    texts <- get_texts ();
    super#reset
end

let add_completion ?action ?wait ?nocase ?(double=true) lb =
  let comp =
    new timed ?wait ?nocase
      (fun () -> Listbox.get_range lb ~first:(`Num 0) ~last:`End) in

  Jg_bind.enter_focus lb;

  bind lb ~events:[`KeyPress] ~fields:[`Char] ~action:
    begin fun ev ->
      (* consider only keys producing characters. The callback is called
         even if you press Shift. *)
      if ev.ev_Char <> "" then
        recenter lb ~index:(`Num (comp#add ev.ev_Char))
    end;

  begin match action with
    Some action ->
      bind lb ~events:[`KeyPressDetail "Return"]
        ~action:(fun _ -> action `Active);
      let bmod = if double then [`Double] else [] in
      bind lb ~events:[`Modified(bmod, `ButtonPressDetail 1)]
        ~breakable:true ~fields:[`MouseY]
        ~action:
        begin fun ev ->
          let index = Listbox.nearest lb ~y:ev.ev_MouseY in
          if not double then begin
            Listbox.selection_clear lb ~first:(`Num 0) ~last:`End;
            Listbox.selection_set lb ~first:index ~last:index;
          end;
          action index;
          break ()
        end
  | None -> ()
  end;

  recenter lb ~index:(`Num 0)   (* so that first item is active *)
