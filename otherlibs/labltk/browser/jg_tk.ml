(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License, with the special exception on linking       *)
(*   described in file ../../../LICENSE.                                 *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

open Tk

let tpos ?(modi=[]) x : textIndex = `Linechar (1,0), `Char x :: modi
and tposend ?(modi=[]) x : textIndex = `End, `Char (-x) :: modi
let tstart : textIndex = `Linechar (1,0), []
and tend : textIndex = `End, []

let wingui = Sys.os_type = "Win32" || Sys.os_type = "Cygwin"
