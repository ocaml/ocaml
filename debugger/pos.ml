(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Damien Doligez, projet Moscova, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2003 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Instruct
open Lexing
open Location
open Events

let get_desc ev =
  let loc = ev.ev_ev.ev_loc in
  let loc_start = loc_start loc and loc_end = loc_end loc in
  Printf.sprintf "file %s, line %d, characters %d-%d"
                 loc_start.pos_fname loc_start.pos_lnum
                 (loc_start.pos_cnum - loc_start.pos_bol + 1)
                 (loc_end.pos_cnum - loc_start.pos_bol + 1)
