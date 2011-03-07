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


let version = "$Id$"

(*
 * Simulate a button with a bitmap AND a label
 *)

let rec sort_options but lab com = function
    [] -> but,lab,com
  |(Command f as o)::l -> sort_options (o::but) lab com l
  |(Bitmap b as o)::l -> sort_options (o::but) lab com l
  |(Text t as o)::l -> sort_options but (o::lab) com l
  |o::l -> sort_options but lab (o::com) l

let create parent options =
  let but,lab,com = sort_options [] [] [] options in
  let f = Frame.create parent com in
  let b = Button.create f (but@com)
  and l = Label.create f (lab@com) in
    pack [b;l][];
    bind l [[],ButtonPressDetail 1] (BindSet ([],(function _ -> Button.invoke b)));
    f

let configure f options =
  let but,lab,com = sort_options [] [] [] options in
  match Pack.slaves f with
    [b;l] ->
      Frame.configure f com;
      Button.configure b (but@com);
      Label.configure l (lab@com)
  | _ -> raise (Invalid_argument "lbutton configure")
