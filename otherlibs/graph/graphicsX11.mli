(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*    Pierre Weis and Jun Furuse, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Additional graphics primitives for the X Windows system. *)

type window_id = string

(** Return the unique identifier of the Caml graphics window.
   The returned string is an unsigned 32 bits integer 
   in decimal form. *)
val window_id : unit -> window_id

(** Create a sub-window of the current Caml graphics window
   and return its identifier. *)
val open_subwindow : x:int -> y:int -> width:int -> height:int -> window_id

(** Close the sub-window having the given identifier. *)
val close_subwindow : window_id -> unit

