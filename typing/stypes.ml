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

(* TO DO:
   (?) reset type names between toplevel phrases
*)

open Format;;
open Lexing;;
open Location;;
open Typedtree;;

type type_info =
    Ti_pat   of pattern
  | Ti_expr  of expression
  | Ti_class of class_expr
  | Ti_mod   of module_expr
;;

let get_location ti =
  match ti with
    Ti_pat p   -> p.pat_loc
  | Ti_expr e  -> e.exp_loc
  | Ti_class c -> c.cl_loc
  | Ti_mod m   -> m.mod_loc
;;

let type_info = ref ([] : type_info list);;

let record ti =
  if !Clflags.save_types && not (get_location ti).Location.loc_ghost then
    type_info := ti :: !type_info
;;

(* A comparison function compatible with inclusion order *)
let compare_loc ti1 ti2 =
  let loc1 = get_location ti1 and loc2 = get_location ti2 in
  match compare loc1.loc_end loc2.loc_end with
  | 0 -> compare loc2.loc_start loc1.loc_start
  | x -> x
;;

let print_position pp pos =
  fprintf pp "%S %d %d %d" pos.pos_fname pos.pos_lnum pos.pos_bol pos.pos_cnum;
;;

let print_info pp ti =
  match ti with
    Ti_class _ | Ti_mod _ -> ()
  | Ti_pat  {pat_loc = loc; pat_type = typ}
  | Ti_expr {exp_loc = loc; exp_type = typ} ->
      print_position pp loc.loc_start;
      fprintf pp " ";
      print_position pp loc.loc_end;
      fprintf pp "@.(@.  ";
      Printtyp.type_expr pp typ;
      fprintf pp "@.)@.";
;;

let get_info () =
  let info = List.sort compare_loc !type_info in
  type_info := [];
  info

let dump filename =
  let info = get_info () in
  if !Clflags.save_types then begin
    let pp = formatter_of_out_channel (open_out filename) in
    List.iter (print_info pp) info
  end;
;;

