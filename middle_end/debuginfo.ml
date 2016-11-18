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

type item = {
  dinfo_file: string;
  dinfo_line: int;
  dinfo_char_start: int;
  dinfo_char_end: int;
}

type t = item list

let none = []

let is_none = function
  | [] -> true
  | _ :: _ -> false

let to_string dbg =
  match dbg with
  | [] -> ""
  | ds ->
    let items =
      List.map
        (fun d ->
           Printf.sprintf "%s:%d,%d-%d"
             d.dinfo_file d.dinfo_line d.dinfo_char_start d.dinfo_char_end)
        ds
    in
    "{" ^ String.concat ";" items ^ "}"

let item_from_location loc =
  { dinfo_file = loc.loc_start.pos_fname;
    dinfo_line = loc.loc_start.pos_lnum;
    dinfo_char_start = loc.loc_start.pos_cnum - loc.loc_start.pos_bol;
    dinfo_char_end =
      if loc.loc_end.pos_fname = loc.loc_start.pos_fname
      then loc.loc_end.pos_cnum - loc.loc_start.pos_bol
      else loc.loc_start.pos_cnum - loc.loc_start.pos_bol;
  }

let from_location loc =
  if loc == Location.none then [] else [item_from_location loc]

let to_location = function
  | [] -> Location.none
  | d :: _ ->
    let loc_start =
      { pos_fname = d.dinfo_file;
        pos_lnum = d.dinfo_line;
        pos_bol = 0;
        pos_cnum = d.dinfo_char_start;
      } in
    let loc_end = { loc_start with pos_cnum = d.dinfo_char_end; } in
    { loc_ghost = false; loc_start; loc_end; }

let inline loc t =
  if loc == Location.none then t
  else (item_from_location loc) :: t

let concat dbg1 dbg2 =
  dbg1 @ dbg2

let compare dbg1 dbg2 =
  let rec loop ds1 ds2 =
    match ds1, ds2 with
    | [], [] -> 0
    | _ :: _, [] -> 1
    | [], _ :: _ -> -1
    | d1 :: ds1, d2 :: ds2 ->
      let c = compare d1.dinfo_file d2.dinfo_file in
      if c <> 0 then c else
      let c = compare d1.dinfo_line d2.dinfo_line in
      if c <> 0 then c else
      let c = compare d1.dinfo_char_end d2.dinfo_char_end in
      if c <> 0 then c else
      let c = compare d1.dinfo_char_start d2.dinfo_char_start in
      if c <> 0 then c else
      loop ds1 ds2
  in
  loop (List.rev dbg1) (List.rev dbg2)
