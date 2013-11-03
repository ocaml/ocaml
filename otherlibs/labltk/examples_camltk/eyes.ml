(***********************************************************************)
(*                                                                     *)
(*                 MLTk, Tcl/Tk interface of OCaml                     *)
(*                                                                     *)
(*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    *)
(*               projet Cristal, INRIA Rocquencourt                    *)
(*            Jacques Garrigue, Kyoto University RIMS                  *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique and Kyoto University.  All rights reserved.         *)
(*  This file is distributed under the terms of the GNU Library        *)
(*  General Public License, with the special exception on linking      *)
(*  described in file LICENSE found in the OCaml source tree.          *)
(*                                                                     *)
(***********************************************************************)

(* The eyes of OCaml (CamlTk) *)

open Camltk;;

let create_eye canvas cx cy wx wy ewx ewy bnd =
  let _oval2 =
    Canvas.create_oval canvas
     (Pixels (cx - wx)) (Pixels (cy - wy))
     (Pixels (cx + wx)) (Pixels (cy + wy))
     [Outline (NamedColor "black"); Width (Pixels 7);
      FillColor (NamedColor "white"); ]
  and oval =
    Canvas.create_oval canvas
     (Pixels (cx - ewx)) (Pixels (cy - ewy))
     (Pixels (cx + ewx)) (Pixels (cy + ewy))
     [FillColor (NamedColor "black")] in
  let curx = ref cx
  and cury = ref cy in

  let treat_event e =

    let xdiff = e.ev_MouseX - cx
    and ydiff = e.ev_MouseY - cy in

    let diff =
      sqrt ((float xdiff /. (float wx *. bnd)) ** 2.0 +.
            (float ydiff /. (float wy *. bnd)) ** 2.0) in

    let nx, ny =
      if diff <= 1.0 then e.ev_MouseX, e.ev_MouseY else
        truncate ((float xdiff) *. (1.0 /. diff)) + cx,
        truncate ((float ydiff) *. (1.0 /. diff)) + cy in

    Canvas.move canvas oval (Pixels (nx - !curx)) (Pixels (ny - !cury));
    curx := nx;
    cury := ny; in

  bind canvas [[], Motion] (
    BindExtend ([Ev_MouseX; Ev_MouseY], treat_event)
  )
;;

let main () =
  let top = opentk () in
  let fw = Frame.create top [] in
  pack [fw] [];

  let canvas = Canvas.create fw [Width (Pixels 200); Height (Pixels 200)] in

  create_eye canvas 60 100 30 40 5 6 0.6;
  create_eye canvas 140 100 30 40 5 6 0.6;
  pack [canvas] [];

  mainLoop ();
;;

Printexc.print main ();;

