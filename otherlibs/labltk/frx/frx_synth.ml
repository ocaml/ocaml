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
(* Some notion of synthetic events *)
open Camltk
open Widget
open Protocol

(* To each event is associated a table of (widget, callback) *)
let events = Hashtbl.create 37

(* Notes:
 *   "cascading" events (on the same event) are not supported 
 *   Only one binding active at a time for each event on each widget.
 *)

(* Get the callback table associated with <name>. Initializes if required *)
let get_event name =
  try Hashtbl.find events name 
  with
    Not_found ->
      let h = Hashtbl.create 37 in
       Hashtbl.add events name h;
       (* Initialize the callback invocation mechanism, based on 
          variable trace
        *)
       let var = "camltk_events(" ^ name ^")" in
       let tkvar = Textvariable.coerce var in
       let rec set () =
         Textvariable.handle tkvar
         (fun () ->
            begin match Textvariable.get tkvar with
              "all" -> (* Invoke all callbacks *)
                Hashtbl.iter
                  (fun p f -> 
                     try 
                      f (cTKtoCAMLwidget p) 
                     with _ -> ())
                  h
            | p -> (* Invoke callback for p *)
                try
                  let w = cTKtoCAMLwidget p
                  and f = Hashtbl.find h p in
                    f w
                with
                  _ -> ()
            end; 
            set ()(* reactivate the callback *)
            ) in
       set();
       h 

(* Remove binding for event <name> on widget <w> *)
let remove w name =   
  Hashtbl.remove (get_event name) (Widget.name w)

(* Adds <f> as callback for widget <w> on event <name> *)
let bind w name f =
  remove w name;
  Hashtbl.add (get_event name) (Widget.name w) f

(* Sends event <name> to all widgets *)
let broadcast name =
  Textvariable.set (Textvariable.coerce ("camltk_events(" ^ name ^")")) "all"

(* Sends event <name> to widget <w> *)
let send name w =
  Textvariable.set (Textvariable.coerce ("camltk_events(" ^ name ^")")) 
                   (Widget.name w)

(* Remove all callbacks associated to widget <w> *)
let remove_callbacks w =
  Hashtbl.iter (fun _ h -> Hashtbl.remove h (Widget.name w)) events

let _ =
  add_destroy_hook remove_callbacks
