(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
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

open Annot;;
open Format;;
open Lexing;;
open Location;;
open Typedtree;;

type annotation =
  | Ti_pat   of pattern
  | Ti_expr  of expression
  | Ti_class of class_expr
  | Ti_mod   of module_expr
  | An_call of Location.t * Annot.call
  | An_ident of Location.t * string * Annot.ident
;;

let get_location ti =
  match ti with
    Ti_pat p   -> p.pat_loc
  | Ti_expr e  -> e.exp_loc
  | Ti_class c -> c.cl_loc
  | Ti_mod m   -> m.mod_loc
  | An_call (l, k) -> l
  | An_ident (l, s, k) -> l
;;

let annotations = ref ([] : annotation list);;
let phrases = ref ([] : Location.t list);;

let record ti =
  if !Clflags.annotations && not (get_location ti).Location.loc_ghost then
    annotations := ti :: !annotations
;;

let record_phrase loc =
  if !Clflags.annotations then phrases := loc :: !phrases;
;;

(* comparison order:
   the intervals are sorted by order of increasing upper bound
   same upper bound -> sorted by decreasing lower bound
*)
let cmp_loc_inner_first loc1 loc2 =
  match compare loc1.loc_end.pos_cnum loc2.loc_end.pos_cnum with
  | 0 -> compare loc2.loc_start.pos_cnum loc1.loc_start.pos_cnum
  | x -> x
;;
let cmp_ti_inner_first ti1 ti2 =
  cmp_loc_inner_first (get_location ti1) (get_location ti2)
;;

let print_position pp pos =
  if pos = dummy_pos then
    fprintf pp "--"
  else
    fprintf pp "%S %d %d %d" pos.pos_fname pos.pos_lnum pos.pos_bol
                             pos.pos_cnum;
;;

let print_location pp loc =
  print_position pp loc.loc_start;
  fprintf pp " ";
  print_position pp loc.loc_end;
;;

let sort_filter_phrases () =
  let ph = List.sort (fun x y -> cmp_loc_inner_first y x) !phrases in
  let rec loop accu cur l =
    match l with
    | [] -> accu
    | loc :: t ->
       if cur.loc_start.pos_cnum <= loc.loc_start.pos_cnum
          && cur.loc_end.pos_cnum >= loc.loc_end.pos_cnum
       then loop accu cur t
       else loop (loc :: accu) loc t
  in
  phrases := loop [] Location.none ph;
;;

let rec printtyp_reset_maybe loc =
  match !phrases with
  | cur :: t when cur.loc_start.pos_cnum <= loc.loc_start.pos_cnum ->
     Printtyp.reset ();
     phrases := t;
     printtyp_reset_maybe loc;
  | _ -> ()
;;

let call_kind_string k =
  match k with
  | Tail -> "tail"
  | Stack -> "stack"
  | Inline -> "inline"
;;

let print_ident_annot pp str k =
  match k with
  | Idef l -> fprintf pp "def %s %a@." str print_location l;
  | Iref_internal l -> fprintf pp "int_ref %s %a@." str print_location l;
  | Iref_external -> fprintf pp "ext_ref %s@." str;
;;

(* The format of the annotation file is documented in emacs/caml-types.el. *)

let print_info pp prev_loc ti =
  match ti with
  | Ti_class _ | Ti_mod _ -> prev_loc
  | Ti_pat  {pat_loc = loc; pat_type = typ; pat_env = env}
  | Ti_expr {exp_loc = loc; exp_type = typ; exp_env = env} ->
      if loc <> prev_loc then fprintf pp "%a@." print_location loc;
      fprintf pp "type(@.  ";
      printtyp_reset_maybe loc;
      Printtyp.mark_loops typ;
      Printtyp.wrap_printing_env env (fun () -> Printtyp.type_sch pp typ);
      fprintf pp "@.)@.";
      loc
  | An_call (loc, k) ->
      if loc <> prev_loc then fprintf pp "%a@." print_location loc;
      fprintf pp "call(@.  %s@.)@." (call_kind_string k);
      loc
  | An_ident (loc, str, k) ->
      if loc <> prev_loc then fprintf pp "%a@." print_location loc;
      fprintf pp "ident(@.  ";
      print_ident_annot pp str k;
      fprintf pp ")@.";
      loc
;;

let get_info () =
  let info = List.fast_sort cmp_ti_inner_first !annotations in
  annotations := [];
  info
;;

let dump filename =
  if !Clflags.annotations then begin
    let info = get_info () in
    let pp =
      match filename with
          None -> std_formatter
        | Some filename -> formatter_of_out_channel (open_out filename) in
    sort_filter_phrases ();
    ignore (List.fold_left (print_info pp) Location.none info);
    phrases := [];
  end else begin
    annotations := [];
  end;
;;
