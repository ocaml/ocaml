(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Damien Doligez, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2003 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Instruct;;
open Lexing;;
open Location;;
open Primitives;;
open Source;;

let get_desc ev =
  let loc = ev.ev_loc in
  if loc.loc_start.pos_fname <> ""
  then Printf.sprintf "file %s, line %d, characters %d-%d"
                      loc.loc_start.pos_fname loc.loc_start.pos_lnum
                      (loc.loc_start.pos_cnum - loc.loc_start.pos_bol + 1)
                      (loc.loc_end.pos_cnum - loc.loc_start.pos_bol + 1)
  else begin
    let filename = source_of_module ev.ev_module in
    try
      let (start, line) = line_of_pos (get_buffer ev.ev_module)
                                      loc.loc_start.pos_cnum
      in
      Printf.sprintf "file %s, line %d, characters %d-%d"
                     filename line (loc.loc_start.pos_cnum - start + 1)
                     (loc.loc_end.pos_cnum - start + 1)
    with Not_found | Out_of_range ->
      Printf.sprintf "file %s, characters %d-%d"
                     filename (loc.loc_start.pos_cnum + 1)
                     (loc.loc_end.pos_cnum + 1)
  end
;;
