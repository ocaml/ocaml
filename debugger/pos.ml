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
open Primitives;;
open Source;;

let get_desc ev =
  if ev.ev_char.pos_fname <> ""
  then Printf.sprintf "file %s, line %d, character %d"
                      ev.ev_char.pos_fname ev.ev_char.pos_lnum
                      (ev.ev_char.pos_cnum - ev.ev_char.pos_bol + 1)
  else begin
    let filename = source_of_module ev.ev_module in
    try
      let (start, line) = line_of_pos (get_buffer ev.ev_module)
                                      ev.ev_char.pos_cnum
      in
      Printf.sprintf "file %s, line %d, character %d"
                     filename line (ev.ev_char.pos_cnum - start + 1)
    with Not_found | Out_of_range ->
      Printf.sprintf "file %s, character %d"
                     filename (ev.ev_char.pos_cnum + 1)
  end
;;
