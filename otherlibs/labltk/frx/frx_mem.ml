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
(* Memory gauge *)
open Camltk
open Gc

let inited = ref None
let w = ref 300
let delay = ref 5 (* in seconds *)
let wordsize = (* officially approved *)
  if 1 lsl 31 = 0 then 4 else 8


let init () = 
  let top = Toplevel.create Widget.default_toplevel [Class "CamlGC"] in
  let name = Camltk.appname_get () in
    Wm.title_set top (name ^ " Memory Gauge");
    Wm.withdraw top;
    inited := Some top;
    (* this should be executed before the internal "all" binding *)
    bind top [[], Destroy] (BindSet ([], (fun _ -> inited := None)));
    let fminors = Frame.create top [] in
      let lminors = Label.create fminors [Text "Minor collections"]
      and vminors = Label.create fminors [] in
      pack [lminors][Side Side_Left];
      pack [vminors][Side Side_Right; Fill Fill_X; Expand true];
    let fmajors = Frame.create top [] in
      let lmajors = Label.create fmajors [Text "Major collections"]
      and vmajors = Label.create fmajors [] in
      pack [lmajors][Side Side_Left];
      pack [vmajors][Side Side_Right; Fill Fill_X; Expand true];
    let fcompacts = Frame.create top [] in
      let lcompacts = Label.create fcompacts [Text "Compactions"]
      and vcompacts = Label.create fcompacts [] in
      pack [lcompacts][Side Side_Left];
      pack [vcompacts][Side Side_Right; Fill Fill_X; Expand true];
    let fsize = Frame.create top [] in
      let lsize = Label.create fsize [Text "Heap size (bytes)"]
      and vsize = Label.create fsize [] in
      pack [lsize][Side Side_Left];
      pack [vsize][Side Side_Right; Fill Fill_X; Expand true];
    let fheap = Frame.create top [Width (Pixels !w); Height (Pixels 10)] in
    let flive = Frame.create fheap [Background Red]
    and ffree = Frame.create fheap [Background Green]
    and fdead = Frame.create fheap [Background Black] in
      pack [fminors; fmajors; fcompacts; fsize; fheap][Fill Fill_X];

    let display () =
      let st = Gc.stat() in
       Label.configure vminors [Text (string_of_int st.minor_collections)];
       Label.configure vmajors [Text (string_of_int st.major_collections)];
       Label.configure vcompacts [Text (string_of_int st.compactions)];
       Label.configure vsize [Text (string_of_int (wordsize * st.heap_words))];
       let liver = (float st.live_words) /. (float st.heap_words)
       and freer = (float st.free_words) /. (float st.heap_words) in
       Place.configure flive [X (Pixels 0); Y (Pixels 0);
                              RelWidth liver; RelHeight 1.0];
       Place.configure ffree [RelX liver; Y (Pixels 0);
                              RelWidth freer; RelHeight 1.0];
       Place.configure fdead [RelX (liver +. freer); Y (Pixels 0);
                              RelWidth (1.0 -. freer -. liver); RelHeight 1.0]

    in
    let rec tim () =
      if Winfo.exists top then begin
        display();
        Timer.set (!delay * 1000) tim
      end
    in
    tim()


let rec f () =
  match !inited with
    Some w -> Wm.deiconify w
  | None -> init (); f()
