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
  We record all types in a list as they are created.
  This means we can dump type information even if type inference fails,
  which is extremely important, since type information is most
  interesting in case of errors.
*)

(*
  TO DO:
   - (?) reset type names between toplevel phrases
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
let phrases = ref ([] : Location.t list);;

let record ti =
  if !Clflags.save_types && not (get_location ti).Location.loc_ghost then
    type_info := ti :: !type_info
;;

let record_phrase loc =
  if !Clflags.save_types then phrases := loc :: !phrases;
;;

(* A comparison function compatible with inclusion order *)
(* if loc1 is included in loc2, then loc1 is greater than loc2 *)
let compare_loc loc1 loc2 =
  match compare loc1.loc_end loc2.loc_end with
  | 0 -> compare loc2.loc_start loc1.loc_start
  | x -> x
;;
let compare_ti ti1 ti2 = compare_loc (get_location ti1) (get_location ti2);;

let print_position pp pos =
  fprintf pp "%S %d %d %d" pos.pos_fname pos.pos_lnum pos.pos_bol pos.pos_cnum;
;;

let sort_filter_phrases () =
  let ph = List.sort (fun x y -> compare_loc y x) !phrases in
  let rec loop accu cur l =
    match l with
    | [] -> accu
    | loc :: t ->
       if compare cur.loc_start loc.loc_start <= 0
          && compare cur.loc_end loc.loc_end >= 0
       then loop accu cur t
       else loop (loc :: accu) loc t
  in
  phrases := loop [] Location.none ph;
;;

let rec printtyp_reset_maybe loc =
  match !phrases with
  | [] -> ()   (* assert false; *)
  | cur :: t ->
      if cur.loc_end <= loc.loc_start then begin
        Printtyp.reset ();
        phrases := t;
        printtyp_reset_maybe loc;
      end;
;;


(* The format of the annotation file is documented in emacs/caml-types.el. *)

let print_info pp ti =
  match ti with
    Ti_class _ | Ti_mod _ -> ()
  | Ti_pat  {pat_loc = loc; pat_type = typ}
  | Ti_expr {exp_loc = loc; exp_type = typ} ->
      print_position pp loc.loc_start;
      fprintf pp " ";
      print_position pp loc.loc_end;
      fprintf pp "@.type(@.  ";
      printtyp_reset_maybe loc;
      Printtyp.mark_loops typ;
      Printtyp.type_expr pp typ;
      fprintf pp "@.)@.";
;;

let get_info () =
  let info = List.fast_sort compare_ti !type_info in
  type_info := [];
  info
;;

let dump filename =
  let info = get_info () in
  if !Clflags.save_types then begin
    let pp = formatter_of_out_channel (open_out filename) in
    sort_filter_phrases ();
    List.iter (print_info pp) info;
    phrases := [];
  end;
;;
