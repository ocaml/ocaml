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

(*
 * Some standard requesters (in Amiga techspeak) or dialog boxes (in Apple
 * jargon).
*)

let version = "$Id$"

(*
 * Simple requester
 *  an entry field, unrestricted, with emacs-like bindings
 * Note: grabs focus, thus always unique at one given moment, and we
 *  shouldn't have to worry about toplevel widget name.
 * We add a title widget in case the window manager does not decorate
 * toplevel windows.
*)

let open_simple title action notaction memory =
  let t = Toplevel.create Widget.default_toplevel [Class "Dialog"] in
  Focus.set t;
  Wm.title_set t title;
  let tit = Label.create t [Text title] in
  let len = max 40 (String.length (Textvariable.get memory)) in
  let e =
    Entry.create t [Relief Sunken; TextVariable memory; TextWidth len] in

  let activate _ =
    let v = Entry.get e in
     Grab.release t;                    (* because of wm *)
     destroy t;                         (* so action can call open_simple *)
     action v in

  bind e [[], KeyPressDetail "Return"] (BindSet ([], activate));

  let f = Frame.create t [] in
  let bok = Button.create f [Text "Ok"; Command activate] in
  let bcancel = Button.create f
            [Text "Cancel";
             Command (fun () -> notaction(); Grab.release t; destroy t)] in

    bind e [[], KeyPressDetail "Escape"]
         (BindSet ([], (fun _ -> Button.invoke bcancel)));
    pack [bok] [Side Side_Left; Expand true];
    pack [bcancel] [Side Side_Right; Expand true];
    pack [tit;e] [Fill Fill_X];
    pack [f] [Side Side_Bottom; Fill Fill_X];
    Frx_widget.resizeable t;
    Focus.set e;
    Tkwait.visibility t;
    Grab.set t

(* A synchronous version *)
let open_simple_synchronous title memory =
  let t = Toplevel.create Widget.default_toplevel [Class "Dialog"] in
  Focus.set t;
  Wm.title_set t title;
  let tit = Label.create t [Text title] in
  let len = max 40 (String.length (Textvariable.get memory)) in
  let e =
    Entry.create t [Relief Sunken; TextVariable memory; TextWidth len] in

  let waiting = Textvariable.create_temporary t in

  let activate _ =
     Grab.release t;                    (* because of wm *)
     destroy t;                         (* so action can call open_simple *)
     Textvariable.set waiting "1" in

  bind e [[], KeyPressDetail "Return"] (BindSet ([], activate));

  let f = Frame.create t [] in
  let bok = Button.create f [Text "Ok"; Command activate] in
  let bcancel =
     Button.create f
        [Text "Cancel";
         Command (fun () ->
                   Grab.release t; destroy t; Textvariable.set waiting "0")] in

    bind e [[], KeyPressDetail "Escape"]
         (BindSet ([], (fun _ -> Button.invoke bcancel)));
    pack [bok] [Side Side_Left; Expand true];
    pack [bcancel] [Side Side_Right; Expand true];
    pack [tit;e] [Fill Fill_X];
    pack [f] [Side Side_Bottom; Fill Fill_X];
    Frx_widget.resizeable t;
    Focus.set e;
    Tkwait.visibility t;
    Grab.set t;
    Tkwait.variable waiting;
    begin match Textvariable.get waiting with
      "1" -> true
    | _ -> false
    end

(*
 * Simple list requester
 * Same remarks as in open_simple.
 * focus seems to be in the listbox automatically
 *)
let open_list title elements action notaction =
  let t = Toplevel.create Widget.default_toplevel [Class "Dialog"] in
  Wm.title_set t title;

  let tit = Label.create t [Text title] in
  let fls = Frame.create t [Relief Sunken; BorderWidth (Pixels 2)] in
  let lb = Listbox.create fls [SelectMode Extended] in
  let sb = Scrollbar.create fls [] in
    Frx_listbox.scroll_link sb lb;
    Listbox.insert lb End elements;

  (* activation: we have to break() because we destroy the requester *)
  let activate _ =
    let l = List.map (Listbox.get lb) (Listbox.curselection lb) in
    Grab.release t;
    destroy t;
    List.iter action l;
    break() in


  bind lb [[Double], ButtonPressDetail 1] (BindSetBreakable ([], activate));

  Frx_listbox.add_completion lb activate;

  let f = Frame.create t [] in
  let bok = Button.create f [Text "Ok"; Command activate] in
  let bcancel = Button.create f
            [Text "Cancel";
             Command (fun () -> notaction(); Grab.release t; destroy t)] in

    pack [bok; bcancel] [Side Side_Left; Fill Fill_X; Expand true];
    pack [lb] [Side Side_Left; Fill Fill_Both; Expand true];
    pack [sb] [Side Side_Right; Fill Fill_Y];
    pack [tit] [Fill Fill_X];
    pack [fls] [Fill Fill_Both; Expand true];
    pack [f] [Side Side_Bottom; Fill Fill_X];
    Frx_widget.resizeable t;
    Tkwait.visibility t;
    Grab.set t


(* Synchronous *)
let open_passwd title =
  let username = ref ""
  and password = ref ""
  and cancelled = ref false in
  let t = Toplevel.create Widget.default_toplevel [Class "Dialog"] in
  Focus.set t;
  Wm.title_set t title;
  let tit = Label.create t [Text title]
  and fu,eu = Frx_entry.new_label_entry t "Username" (fun s -> ())
  and fp,ep = Frx_entry.new_label_entry t "Password" (fun s -> ())
  in
  let fb = Frame.create t [] in
   let bok = Button.create fb
              [Text "Ok"; Command (fun _ ->
                                    username := Entry.get eu;
                                    password := Entry.get ep;
                                    Grab.release t; (* because of wm *)
                                    destroy t)] (* will return from tkwait *)
   and bcancel = Button.create fb
              [Text "Cancel"; Command (fun _ ->
                                    cancelled := true;
                                    Grab.release t; (* because of wm *)
                                    destroy t)] (* will return from tkwait *)
  in
    Entry.configure ep [Show '*'];
    bind eu [[], KeyPressDetail "Return"]
      (BindSetBreakable ([], (fun _ -> Focus.set ep; break())));
    bind ep [[], KeyPressDetail "Return"]
      (BindSetBreakable ([], (fun _ -> Button.flash bok;
                                       Button.invoke bok;
                                       break())));

    pack [bok] [Side Side_Left; Expand true];
    pack [bcancel] [Side Side_Right; Expand true];
    pack [tit;fu;fp;fb] [Fill Fill_X];
    Tkwait.visibility t;
    Focus.set eu;
    Grab.set t;
    Tkwait.window t;
    if !cancelled then failwith "cancelled"
    else (!username, !password)
