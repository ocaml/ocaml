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

(* $Id$ *)

open StdLabels

(* easy balloon help facility *)

open Tk
open Widget
open Protocol
open Support

(* switch -- if you do not want balloons, set false *)
let flag = ref true
let debug = ref false

(* We assume we have at most one popup label at a time *)
let topw = ref default_toplevel
and popupw = ref (Obj.magic dummy : message widget)

let configure_cursor w cursor =
  (* DDDDDDDDDIIIIIIIRRRRRRRRTTTTTTTTYYYYYYY *)
  Protocol.tkCommand [| TkToken (name w);
                    TkToken "configure";
                    TkToken "-cursor";
                    TkToken cursor |]

let put ~on: w ~ms: millisec mesg =
  let t = ref None in
  let cursor = ref "" in

  let reset () =
      begin
        match !t with
          Some t -> Timer.remove t
        | _ -> ()
      end;
      (* if there is a popup label, unmap it *)
      if Winfo.exists !topw && Wm.state !topw <> "withdrawn" then
        begin
          Wm.withdraw !topw;
          if Winfo.exists w then configure_cursor w !cursor
        end
  and set ev =
    if !flag then
      t := Some (Timer.add ~ms: millisec ~callback: (fun () ->
        t := None;
        if !debug then
          prerr_endline ("Balloon: " ^ Widget.name w);
        update_idletasks();
        Message.configure !popupw ~text: mesg;
        raise_window !topw;
        Wm.geometry_set !topw (* 9 & 8 are some kind of magic... *)
          ("+"^(string_of_int (ev.ev_RootX + 9))^
           "+"^(string_of_int (ev.ev_RootY + 8)));
        Wm.deiconify !topw;
        cursor := cget w `Cursor;
        configure_cursor w "hand2"))
  in

  List.iter [[`Leave]; [`ButtonPress]; [`ButtonRelease]; [`Destroy];
             [`KeyPress]; [`KeyRelease]]
    ~f:(fun events -> bind w ~events ~extend:true ~action:(fun _ -> reset ()));
  List.iter [[`Enter]; [`Motion]] ~f:
    begin fun events ->
      bind w ~events ~extend:true ~fields:[`RootX; `RootY]
        ~action:(fun ev -> reset (); set ev)
    end

let init () =
  let t = Hashtbl.create 101 in
  Protocol.add_destroy_hook (fun w ->
    Hashtbl.remove t w);
  topw := Toplevel.create default_toplevel;
  Wm.overrideredirect_set !topw true;
  Wm.withdraw !topw;
  popupw := Message.create !topw ~name: "balloon"
              ~background: (`Color "yellow") ~aspect: 300;
  pack [!popupw];
  bind_class "all" ~events: [`Enter] ~extend:true ~fields:[`Widget] ~action:
    begin fun w ->
      try Hashtbl.find t w.ev_Widget
      with Not_found ->
        Hashtbl.add t w.ev_Widget ();
        let x = Option.get w.ev_Widget ~name: "balloon" ~clas: "Balloon" in
        if x <> "" then put ~on: w.ev_Widget ~ms: 1000 x
    end
