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
(* A selection handler *)
open Widget
open Protocol
open Camltk

let frame = ref None
let selection = ref ""

let read ofs n =
  let res =
    if ofs < 0 then ""
    else if ofs + n > String.length !selection
    then String.sub !selection ofs (String.length !selection - ofs)
    else String.sub !selection ofs n in
   tkreturn res

(* As long as we don't loose the selection, we keep the widget *)
(* Calling this function means that we own the selection       *)
(* When we loose the selection, both cb are destroyed *)
let own () =
  match !frame with
    None ->
      let f = Frame.create_named Widget.default_toplevel "frx_selection" [] in
       let lost () = selection := ""; destroy f; frame := None in
       Selection.own_set [Selection "PRIMARY"; LostCommand lost] f;
       Selection.handle_set [Selection "PRIMARY"; ICCCMType "STRING"] f read;
       frame := Some f
  | Some f -> ()

let set s = own(); selection := s
