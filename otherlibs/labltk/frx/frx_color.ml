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
open Protocol

module StringSet = Set.Make(struct type t = string let compare = compare end)

(* should we keep a negative cache ? *)
let available_colors = ref (StringSet.empty)

let check s =
  if StringSet.mem s !available_colors then true
  else begin
    try
      let f = Frame.create_named Widget.default_toplevel "frxcolorcheck" 
          [Background (NamedColor s)] in
      available_colors := StringSet.add s !available_colors;
      destroy f;
      true
    with
      TkError _ -> false
  end
