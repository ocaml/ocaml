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
 * Finding fonts. Inspired by code in Ical by Sanjay Ghemawat.
 * Possibly bogus because some families use "i" for italic where others
 * use "o".
 * wght: bold, medium
 * slant: i, o, r
 * pxlsz: 8, 10, ...
*)
module StringSet = Set.Make(struct type t = string let compare = compare end)

let available_fonts = ref (StringSet.empty)

let get_canvas = 
  Frx_misc.autodef (fun () -> Canvas.create Widget.default_toplevel [])


let find fmly wght slant pxlsz =
  let fontspec =
     "-*-"^fmly^"-"^wght^"-"^slant^"-normal-*-"^string_of_int pxlsz^"-*-*-*-*-*-iso8859-1" in
    if StringSet.mem fontspec !available_fonts then fontspec
    else
      let c = get_canvas() in
      try
        let tag = Canvas.create_text c (Pixels 0) (Pixels 0) 
                                [Text "foo"; Font fontspec] in
           Canvas.delete c [tag];
           available_fonts := StringSet.add fontspec !available_fonts;
           fontspec
      with
        _ -> raise (Invalid_argument fontspec)

