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
open Widget
open Tkanim
open Tk

let main () =
  let file = ref "" in
    Arg.parse [] (fun s -> file := s)
      "usage: gifanimtest file (animated gif)\n\
       \tbutton 1 toggles the animation,\n\
       \tbutton 2 displays the next frame,\n\
       \tbutton 3 quits.";
    let t = openTk () in

      (* First of all, you must initialize the extension. *) 
      Tkanim.init ();

      prerr_endline !file;

      (* Then load the animated gif. *)
      let anim = Tkanim.create !file in  
      prerr_endline "load done";

      (* Check it is really animated or not. *)
      match anim with
      | Still x -> 
          (* Use whatever you want in CamlTk with this ImagePhoto. *)
          prerr_endline "Sorry, it is not an animated GIF."

      | Animated x ->
          (* OK, let's animate it. *)
          let l = Label.create t [] in
            pack [l] [];
          
            (* animate returns an interface function. *)
            let f = animate l x in

              (* Button1 toggles the animation *)
              bind l [[], ButtonPressDetail 1] (BindSet ([], (fun _ ->
                f false)));

              (* Button2 displays the next frame. *)
              bind l [[], ButtonPressDetail 2] (BindSet ([], (fun _ ->
                f true)));

              (* Button3 quits. *)
              bind l [[], ButtonPressDetail 3] (BindSet ([], (fun _ ->
                closeTk ())));

              (* start the animation *)
              f false;

              (* Go to the main loop. *)
              mainLoop ()
    
let _ = Printexc.print main ()
