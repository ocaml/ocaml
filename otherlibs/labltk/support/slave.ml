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

(* The code run on initialisation, in addition to normal Tk code
 * NOTE: camltk has not fully been initialised yet
 *)
external tcl_eval : string -> string
        =  "camltk_tcl_eval"
let tcl_command s = ignore (tcl_eval s);;
open Printf

let dynload args =
  List.iter Dynlink.loadfile args

(* Default modules include everything from 
let default_modules = []
*)

(* [caml::run foo.cmo .. bar.cmo] is now available from Tcl *)
let init () =
  Dynlink.init();
  (* Make it unsafe by default, with everything available *)
  Dynlink.allow_unsafe_modules true;
  Dynlink.add_interfaces [] [];
  let s = register_callback Widget.dummy dynload in
  tcl_command (sprintf "proc caml::run {l} {camlcb %s l}" s)

let _ =
  Printexc.print init ()

(* A typical master program would then
 *   caml::run foo.cmo
 *     # during initialisation, "foo" was registered as a tcl procedure
 *   foo x y z
 *     # proceed with some Tcl code calling foo
 *)
