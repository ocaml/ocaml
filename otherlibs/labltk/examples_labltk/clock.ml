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

(* Clock/V, a simple clock.
   Reverts every time you push the right button.
   Adapted from ASCII/V May 1997

   Uses Tk and Unix, so you must link with
     labltklink unix.cma clock.ml -o clock -cclib -lunix
*)

open Tk

(* pi is not a constant! *)
let pi = acos (-1.)

(* The main class:
     * create it with a parent: [new clock parent:top]
     * initialize with [#init]
*)

class clock ~parent = object (self)

  (* Instance variables *)
  val canvas = Canvas.create ~width:100 ~height:100 parent
  val mutable height = 100
  val mutable width = 100
  val mutable rflag = -1

  (* Convert from -1.0 .. 1.0 to actual positions on the canvas *)
  method x x0 = truncate (float width *. (x0 +. 1.) /. 2.)
  method y y0 = truncate (float height *. (y0 +. 1.) /. 2.)

  initializer
    (* Create the oval border *)
    Canvas.create_oval canvas ~tags:["cadran"]
      ~x1:1 ~y1:1 ~x2:(width - 2) ~y2:(height - 2)
      ~width:3 ~outline:`Yellow ~fill:`White;
    (* Draw the figures *)
    self#draw_figures;
    (* Create the arrows with dummy position *)
    Canvas.create_line canvas
      ~xys:[self#x 0., self#y 0.; self#x 0., self#y 0.]
      ~tags:["hours"] ~fill:`Red;
    Canvas.create_line canvas
      ~xys:[self#x 0., self#y 0.; self#x 0., self#y 0.]
      ~tags:["minutes"] ~fill:`Blue;
    Canvas.create_line canvas
      ~xys:[self#x 0., self#y 0.; self#x 0., self#y 0.]
      ~tags:["seconds"] ~fill:`Black;
    (* Setup a timer every second *)
    let rec timer () =
      self#draw_arrows (Unix.localtime (Unix.time ()));
      Timer.add ~ms:1000 ~callback:timer; ()
    in timer ();
    (* Redraw when configured (changes size) *)
    bind canvas ~events:[`Configure] ~action:
      begin fun _ ->
        width <- Winfo.width canvas;
        height <- Winfo.height canvas;
        self#redraw
      end;
    (* Change direction with right button *)
    bind canvas ~events:[`ButtonPressDetail 3]
      ~action:(fun _ -> rflag <- -rflag; self#redraw);
    (* Pack, expanding in both directions *)
    pack ~fill:`Both ~expand:true [canvas]

  (* Redraw everything *)
  method redraw =
    Canvas.coords_set canvas (`Tag "cadran")
      ~xys:[ 1, 1; width - 2, height - 2 ];
    self#draw_figures;
    self#draw_arrows (Unix.localtime (Unix.time ()))

  (* Delete and redraw the figures *)
  method draw_figures =
    Canvas.delete canvas [`Tag "figures"];
    for i = 1 to 12 do
      let angle = float (rflag * i - 3) *. pi /. 6. in
      Canvas.create_text canvas
        ~x:(self#x (0.8 *. cos angle)) ~y:(self#y (0.8 *. sin angle))
        ~tags:["figures"]
        ~text:(string_of_int i) ~font:"variable"
        ~anchor:`Center
    done

  (* Resize and reposition the arrows *)
  method draw_arrows tm =
    Canvas.configure_line ~width:(min width height / 40)
      canvas (`Tag "hours");
    let hangle =
      float (rflag * (tm.Unix.tm_hour * 60 + tm.Unix.tm_min) - 180)
        *. pi /. 360. in
    Canvas.coords_set canvas (`Tag "hours")
      ~xys:[ self#x 0., self#y 0.;
             self#x (cos hangle /. 2.), self#y (sin hangle /. 2.) ];
    Canvas.configure_line ~width:(min width height / 50)
      canvas (`Tag "minutes");
    let mangle = float (rflag * tm.Unix.tm_min - 15) *. pi /. 30. in
    Canvas.coords_set canvas (`Tag "minutes")
      ~xys:[ self#x 0., self#y 0.;
             self#x (cos mangle /. 1.5), self#y (sin mangle /. 1.5) ];
    let sangle = float (rflag * tm.Unix.tm_sec - 15) *. pi /. 30. in
    Canvas.coords_set canvas (`Tag "seconds")
      ~xys:[ self#x 0., self#y 0.;
             self#x (cos sangle /. 1.25), self#y (sin sangle /. 1.25) ]
end

(* Initialize the Tcl interpreter *)
let top = openTk ()

(* Create a clock on the main window *)
let clock =
  new clock ~parent:top

(* Wait for events *)
let _ = mainLoop ()
