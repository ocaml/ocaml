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

let main () =
  let top = opentk ()  in
  (* The widgets. They all have "top" as parent widget. *)
  let en1 = Entry.create top [TextWidth 6; Relief Sunken] in
  let lab1 = Label.create top [Text "plus"] in
  let en2 = Entry.create top [TextWidth 6 ; Relief Sunken] in
  let lab2 = Label.create top [Text "="] in
  let result_display = Label.create top [] in
  (* References holding values of entry widgets *)
  let n1 = ref 0
  and n2 = ref 0  in
  (* Refresh result *)
  let refresh () =
    Label.configure result_display [Text (string_of_int (!n1 + !n2))]  in
  (* Electric *)
  let get_and_refresh (w,r) =
    fun _ _ ->
      try
       r := int_of_string (Entry.get w);
       refresh ()
      with
        Failure "int_of_string" ->
          Label.configure result_display [Text "error"]
  in
  (* Set the callbacks *)
  Entry.configure en1 [XScrollCommand (get_and_refresh (en1,n1)) ];
  Entry.configure en2 [XScrollCommand (get_and_refresh (en2,n2)) ];
  (* Map the widgets *)
  pack [en1;lab1;en2;lab2;result_display] [];
  (* Make the window resizable *)
  Wm.minsize_set top 1 1;
  (* Start interaction (event-driven program) *)
  mainLoop ()
;;

let _ = Printexc.catch main () ;;
