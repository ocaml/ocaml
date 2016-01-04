(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2006 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Lexing
open Location


type kind = Dinfo_call | Dinfo_raise | Dinfo_inline of t

and t = {
  dinfo_kind: kind;
  dinfo_file: string;
  dinfo_line: int;
  dinfo_char_start: int;
  dinfo_char_end: int
}

let none = {
  dinfo_kind = Dinfo_call;
  dinfo_file = "";
  dinfo_line = 0;
  dinfo_char_start = 0;
  dinfo_char_end = 0
}

(* PR#5643: cannot use (==) because Debuginfo values are marshalled *)
let is_none t =
  t = none

let to_string d =
  let rec to_list d =
    if is_none d then []
    else
      let s =
        Printf.sprintf "%s:%d,%d-%d"
          d.dinfo_file d.dinfo_line d.dinfo_char_start d.dinfo_char_end
      in
      match d.dinfo_kind with
      | Dinfo_inline d' -> s :: to_list d'
      | _ -> [s]
  in
  match to_list d with
  | [] -> ""
  | ds -> "{" ^ String.concat ";" ds ^ "}"

let from_filename kind filename = {
  dinfo_kind = kind;
  dinfo_file = filename;
  dinfo_line = 0;
  dinfo_char_start = 0;
  dinfo_char_end = 0
}

let from_location kind loc =
  if loc == Location.none then none else
  { dinfo_kind = kind;
    dinfo_file = loc.loc_start.pos_fname;
    dinfo_line = loc.loc_start.pos_lnum;
    dinfo_char_start = loc.loc_start.pos_cnum - loc.loc_start.pos_bol;
    dinfo_char_end =
      if loc.loc_end.pos_fname = loc.loc_start.pos_fname
      then loc.loc_end.pos_cnum - loc.loc_start.pos_bol
      else loc.loc_start.pos_cnum - loc.loc_start.pos_bol }

let from_call ev = from_location Dinfo_call ev.Lambda.lev_loc
let from_raise ev = from_location Dinfo_raise ev.Lambda.lev_loc

let to_location d =
  if is_none d then Location.none
  else
    let loc_start =
      { pos_fname = d.dinfo_file;
        pos_lnum = d.dinfo_line;
        pos_bol = 0;
        pos_cnum = d.dinfo_char_start;
      } in
    let loc_end = { loc_start with pos_cnum = d.dinfo_char_end; } in
    { loc_ghost = false; loc_start; loc_end; }

let inline loc t =
  if loc = Location.none then
    t
  else
    from_location (Dinfo_inline t) loc

let concat dbg1 dbg2 =
  if is_none dbg1 then dbg2
  else if is_none dbg2 then dbg1
  else
    let rec aux dbg1 dbg2 =
      match dbg1.dinfo_kind with
      | Dinfo_call ->
          {dbg1 with dinfo_kind = Dinfo_inline dbg2}
      | Dinfo_raise ->
          invalid_arg "Debuginfo.concat: inlining from a raise site"
      | Dinfo_inline dbg1' ->
          {dbg1 with dinfo_kind = Dinfo_inline (aux dbg1' dbg2)}
    in
    aux dbg1 dbg2

let unroll_inline_chain t =
  let rec aux acc t = match t.dinfo_kind with
    | Dinfo_inline t' ->
        aux ({t with dinfo_kind = Dinfo_call} :: acc) t'
    | _ -> t, acc
  in
  aux [] t
