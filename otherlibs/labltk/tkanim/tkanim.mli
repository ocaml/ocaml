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
open Support

(*** Data types ***)

type animatedGif

    (* This data type contains all the information of an animation of
       gif89a format. It is still test implementation, so I should 
       keep it abstract. --- JPF *)

type imageType =
  | Still of Tk.options
  | Animated of animatedGif

      (* This data type is required to distinguish normal still images
         and animated gifs. Usually objects typed imagePhoto or
         imageBitmap are used for Still. *)

(*** Flags ***)

val debug : bool ref 

(*** Library availability check ***)

val init : unit -> unit

    (* This function calls the initialization function for Tkanim
       Tcl/Tk extension. *)

val available : unit -> bool

      (* [available ()] returns true if there is Tkanim Tcl/Tk
         extension linked statically/dynamically in Tcl/Tk
         interpreter. Otherwise, return false. *)

(*** User interface ***)

(* create is unsafe *)
val create : string -> imageType

      (* [create file] loads a gif87 or gif89 image file and parse it,
         and returns [Animated animated_gif] if the image file has
         more than one images. Otherwise, it returns 
         [Still (ImagePhoto image_photo)] *) 

val delete : animatedGif -> unit

      (* [delete anim] deletes all the images in anim. Usually
         animatedGifs contain many images, so you must not forget to
         use this function to free the memory. *)

val width : animatedGif -> int
val height : animatedGif -> int
      (* [width anim] and [height anim] return the width and height of
         given animated gif. *)

val images : animatedGif -> imagePhoto list
      (* [images anim] returns the list of still images used in the 
         animation *)

val animate : widget -> animatedGif -> bool -> unit
val animate_canvas_item : widget -> tagOrId -> animatedGif -> bool -> unit
      (* The display functions for animated gifs. Since [animatedGif] is
         an abstract type, you must use those functions to display
         [animatedGif] images.
         [animate label anim] and [animate_canvas_item canvas tag anim]
         display animation [anim] on a label widget [label] or an
         image tag [tag] on a canvas widget [canvas] respectively.

         Note that animation is stopped by default.
         These functions return interface functions, say, [inter :
         bool -> unit]. Currently, [inter false] toggles start/stop of
         the animation, and [inter true] displays the next frame of
         the animation if the animation is stopped. *)

val gifdata : string -> imageType
      (* [gifdata data] reads [data] as a row data of a gif file and
         decodes it. *)
