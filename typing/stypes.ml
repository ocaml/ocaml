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

(* Recording and dumping (partial) type information *)

(*
  Saving and dumping type information.
  We record all types in a list as they are created.
  This means we can dump type information even if type inference fails,
  which is extremely important, since type information is most
  interesting in case of errors.
*)

open Format;;
open Lexing;;
open Location;;

let type_info = ref [];;

let record loc typ =
  if not loc.Location.loc_ghost then type_info := (loc, typ) :: !type_info;
;;

(* A comparison function compatible with inclusion order *)
let compare_loc (loc1, _) (loc2, _) =
  match compare loc1.loc_end loc2.loc_end with
  | 0 -> compare loc2.loc_start loc1.loc_start
  | x -> x
;;

let print_position pp pos =
  fprintf pp "%S %d %d %d" pos.pos_fname pos.pos_lnum pos.pos_bol pos.pos_cnum;
;;

let print_info pp (loc, typ) =
  print_position pp loc.loc_start;
  fprintf pp " ";
  print_position pp loc.loc_end;
  fprintf pp "@.(@.  ";
  Printtyp.type_expr pp typ;
  fprintf pp "@.)@.";
;;

let dump filename =
  let info = List.sort compare_loc !type_info in
  type_info := [];
  if !Clflags.save_types then begin
    let pp = formatter_of_out_channel (open_out filename) in
    List.iter (print_info pp) info
  end;
;;
