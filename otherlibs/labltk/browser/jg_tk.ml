(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

open Tk

let tpos x : textIndex = `Linechar (1,0), [`Char x]
and tposend x : textIndex = `End, [`Char (-x)]
let tstart : textIndex = `Linechar (1,0), []
and tend : textIndex = `End, []
