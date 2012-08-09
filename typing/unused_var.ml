(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*           Damien Doligez, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Parsetree

let silent v = String.length v > 0 && v.[0] = '_';;

let add_vars tbl (vll1, vll2) =
  let add_var (v, _loc, used) = Hashtbl.add tbl v used in
  List.iter add_var vll1;
  List.iter add_var vll2;
;;

let rm_vars tbl (vll1, vll2) =
  let rm_var (v, _, _) = Hashtbl.remove tbl v in
  List.iter rm_var vll1;
  List.iter rm_var vll2;
;;

let w_suspicious x = Warnings.Unused_var x;;
let w_strict x = Warnings.Unused_var_strict x;;

let check_rm_vars ppf tbl (vlul_pat, vlul_as) =
  let check_rm_var kind (v, loc, used) =
    if not !used && not (silent v)
    then Location.print_warning loc ppf (kind v);
    Hashtbl.remove tbl v;
  in
  List.iter (check_rm_var w_strict) vlul_pat;
  List.iter (check_rm_var w_suspicious) vlul_as;
;;

let check_rm_let ppf tbl vlulpl =
  let check_rm_one flag (v, loc, used) =
    Hashtbl.remove tbl v;
    flag && (silent v || not !used)
  in
  let warn_var w_kind (v, loc, used) =
    if not (silent v) && not !used
    then Location.print_warning loc ppf (w_kind v)
  in
  let check_rm_pat (def, def_as) =
    let def_unused = List.fold_left check_rm_one true def in
    let all_unused = List.fold_left check_rm_one def_unused def_as in
    List.iter (warn_var (if all_unused then w_suspicious else w_strict)) def;
    List.iter (warn_var w_suspicious) def_as;
  in
  List.iter check_rm_pat vlulpl;
;;

let rec get_vars ((vacc, asacc) as acc) p =
  match p.ppat_desc with
  | Ppat_any -> acc
  | Ppat_var v -> ((v, p.ppat_loc, ref false) :: vacc, asacc)
  | Ppat_alias (pp, v) ->
      get_vars (vacc, ((v, p.ppat_loc, ref false) :: asacc)) pp
  | Ppat_constant _ -> acc
  | Ppat_tuple pl -> List.fold_left get_vars acc pl
  | Ppat_construct (_, po, _) -> get_vars_option acc po
  | Ppat_variant (_, po) -> get_vars_option acc po
  | Ppat_record (ipl, cls) ->
      List.fold_left (fun a (_, p) -> get_vars a p) acc ipl
  | Ppat_array pl -> List.fold_left get_vars acc pl
  | Ppat_or (p1, _p2) -> get_vars acc p1
  | Ppat_lazy p -> get_vars acc p
  | Ppat_constraint (pp, _) -> get_vars acc pp
  | Ppat_type _ -> acc
  | Ppat_unpack _ -> acc

and get_vars_option acc po =
  match po with
  | Some p -> get_vars acc p
  | None -> acc
;;

let get_pel_vars pel =
  List.map (fun (p, _) -> get_vars ([], []) p) pel
;;

(*>JOCAML*)

let get_jpat_vars acc jpat =
  let _, p =  jpat.pjpat_desc in
  get_vars acc p

let get_jpats_vars = List.fold_left get_jpat_vars

let get_jpat_chan acc jpat =
  let jident,_ = jpat.pjpat_desc in
  let v = jident.pjident_desc in
  if List.exists (fun (w,_,_) -> v = w) acc then
    acc
  else
    (v, jident.pjident_loc, ref false)::acc

let get_clause_chans acc cl =
  let pats,_ = cl.pjclause_desc in
  List.fold_left get_jpat_chan acc pats

let get_def_chans acc d =
  List.fold_left get_clause_chans acc d.pjauto_desc

let get_defs_chans ds = List.fold_left get_def_chans [] ds

(*<JOCAML*)

let rec structure ppf tbl l =
  List.iter (structure_item ppf tbl) l

and structure_item ppf tbl s =
  match s.pstr_desc with
  | Pstr_eval e -> expression ppf tbl e;
  | Pstr_value (recflag, pel) -> let_pel ppf tbl recflag pel None;
  | Pstr_primitive _ -> ()
  | Pstr_type _ -> ()
  | Pstr_exception _ -> ()
  | Pstr_exn_rebind _ -> ()
  | Pstr_module (_, me) -> module_expr ppf tbl me;
  | Pstr_recmodule stml ->
      List.iter (fun (_, _, me) -> module_expr ppf tbl me) stml;
  | Pstr_modtype _ -> ()
  | Pstr_open _ -> ()
  | Pstr_class cdl -> List.iter (class_declaration ppf tbl) cdl;
  | Pstr_class_type _ -> ()
  | Pstr_include me -> module_expr ppf tbl me;
(*>JOCAML*)
  | Pstr_exn_global _ -> ()
  | Pstr_def d -> join_defs ppf tbl d None
(*<JOCAML*)

and expression ppf tbl e =
  match e.pexp_desc with
  | Pexp_ident (Longident.Lident id) ->
      begin try (Hashtbl.find tbl id) := true;
      with Not_found -> ()
      end;
  | Pexp_ident _ -> ()
  | Pexp_constant _ -> ()
  | Pexp_let (recflag, pel, e) ->
      let_pel ppf tbl recflag pel (Some (fun ppf tbl -> expression ppf tbl e));
  | Pexp_function (_, eo, pel) ->
      expression_option ppf tbl eo;
      match_pel ppf tbl pel;
  | Pexp_apply (e, lel) ->
      expression ppf tbl e;
      List.iter (fun (_, e) -> expression ppf tbl e) lel;
  | Pexp_match (e, pel) ->
      expression ppf tbl e;
      match_pel ppf tbl pel;
  | Pexp_try (e, pel) ->
      expression ppf tbl e;
      match_pel ppf tbl pel;
  | Pexp_tuple el -> List.iter (expression ppf tbl) el;
  | Pexp_construct (_, eo, _) -> expression_option ppf tbl eo;
  | Pexp_variant (_, eo) -> expression_option ppf tbl eo;
  | Pexp_record (iel, eo) ->
      List.iter (fun (_, e) -> expression ppf tbl e) iel;
      expression_option ppf tbl eo;
  | Pexp_field (e, _) -> expression ppf tbl e;
  | Pexp_setfield (e1, _, e2) ->
      expression ppf tbl e1;
      expression ppf tbl e2;
  | Pexp_array el -> List.iter (expression ppf tbl) el;
  | Pexp_ifthenelse (e1, e2, eo) ->
      expression ppf tbl e1;
      expression ppf tbl e2;
      expression_option ppf tbl eo;
  | Pexp_sequence (e1, e2) ->
      expression ppf tbl e1;
      expression ppf tbl e2;
  | Pexp_while (e1, e2) ->
      expression ppf tbl e1;
      expression ppf tbl e2;
  | Pexp_for (id, e1, e2, _, e3) ->
      expression ppf tbl e1;
      expression ppf tbl e2;
      let defined = ([ (id, e.pexp_loc, ref true) ], []) in
      add_vars tbl defined;
      expression ppf tbl e3;
      check_rm_vars ppf tbl defined;
  | Pexp_constraint (e, _, _) -> expression ppf tbl e;
  | Pexp_when (e1, e2) ->
      expression ppf tbl e1;
      expression ppf tbl e2;
  | Pexp_send (e, _) -> expression ppf tbl e;
  | Pexp_new _ -> ()
  | Pexp_setinstvar (_, e) -> expression ppf tbl e;
  | Pexp_override sel -> List.iter (fun (_, e) -> expression ppf tbl e) sel;
  | Pexp_letmodule (_, me, e) ->
      module_expr ppf tbl me;
      expression ppf tbl e;
  | Pexp_assert e -> expression ppf tbl e;
  | Pexp_assertfalse -> ()
  | Pexp_lazy e -> expression ppf tbl e;
  | Pexp_poly (e, _) -> expression ppf tbl e;
  | Pexp_object cs -> class_structure ppf tbl cs;
(*>JOCAML*)
  | Pexp_def (d,e) ->
      join_defs ppf tbl d (Some (fun ppf tbl -> expression ppf tbl e))
  | Pexp_reply (e, _) -> expression ppf tbl e
  | Pexp_par (e1, e2) ->
       expression ppf tbl e1 ;
       expression ppf tbl e2
  | Pexp_spawn e -> expression ppf tbl e
(*<JOCAML*)
  | Pexp_newtype (_, e) -> expression ppf tbl e
  | Pexp_pack me -> module_expr ppf tbl me
  | Pexp_open (_, e) -> expression ppf tbl e

and expression_option ppf tbl eo =
  match eo with
  | Some e -> expression ppf tbl e;
  | None -> ()

and let_pel ppf tbl recflag pel body =
  match recflag with
  | Asttypes.Recursive ->
      let defined = get_pel_vars pel in
      List.iter (add_vars tbl) defined;
      List.iter (fun (_, e) -> expression ppf tbl e) pel;
      begin match body with
      | None ->
          List.iter (rm_vars tbl) defined;
      | Some f ->
          f ppf tbl;
          check_rm_let ppf tbl defined;
      end;
  | _ ->
      List.iter (fun (_, e) -> expression ppf tbl e) pel;
      begin match body with
      | None -> ()
      | Some f ->
          let defined = get_pel_vars pel in
          List.iter (add_vars tbl) defined;
          f ppf tbl;
          check_rm_let ppf tbl defined;
      end;

(*>JOCAML*)
and join_clause ppf tbl cl =
  let jpats,e = cl.pjclause_desc in
  let defined = get_jpats_vars ([],[]) jpats in
  add_vars tbl defined ;
  expression ppf tbl e ;
  check_rm_vars ppf tbl defined

and join_def ppf tbl d =
  List.iter (join_clause ppf tbl) d.pjauto_desc

and join_defs ppf tbl ds body =
  let defined = (get_defs_chans ds,[]) in
  add_vars tbl defined ;
  List.iter (fun d -> join_def ppf tbl d)  ds ;
  begin match body with
  | None ->
      rm_vars tbl defined
  | Some f ->
      f ppf tbl ;
      check_rm_let ppf tbl [defined]
  end
(*<JOCAML*)

and match_pel ppf tbl pel =
  List.iter (match_pe ppf tbl) pel

and match_pe ppf tbl (p, e) =
 let defined = get_vars ([], []) p in
  add_vars tbl defined;
  expression ppf tbl e;
  check_rm_vars ppf tbl defined;

and module_expr ppf tbl me =
  match me.pmod_desc with
  | Pmod_ident _ -> ()
  | Pmod_structure s -> structure ppf tbl s
  | Pmod_functor (_, _, me) -> module_expr ppf tbl me
  | Pmod_apply (me1, me2) ->
      module_expr ppf tbl me1;
      module_expr ppf tbl me2;
  | Pmod_constraint (me, _) -> module_expr ppf tbl me
  | Pmod_unpack (e) -> expression ppf tbl e

and class_declaration ppf tbl cd = class_expr ppf tbl cd.pci_expr

and class_expr ppf tbl ce =
  match ce.pcl_desc with
  | Pcl_constr _ -> ()
  | Pcl_structure cs -> class_structure ppf tbl cs;
  | Pcl_fun (_, _, _, ce) -> class_expr ppf tbl ce;
  | Pcl_apply (ce, lel) ->
      class_expr ppf tbl ce;
      List.iter (fun (_, e) -> expression ppf tbl e) lel;
  | Pcl_let (recflag, pel, ce) ->
      let_pel ppf tbl recflag pel (Some (fun ppf tbl -> class_expr ppf tbl ce));
  | Pcl_constraint (ce, _) -> class_expr ppf tbl ce;

and class_structure ppf tbl (p, cfl) =
  let defined = get_vars ([], []) p in
  add_vars tbl defined;
  List.iter (class_field ppf tbl) cfl;
  check_rm_vars ppf tbl defined;

and class_field ppf tbl cf =
  match cf with
  | Pcf_inher (_, ce, _) -> class_expr ppf tbl ce;
  | Pcf_val (_, _, _, e, _) -> expression ppf tbl e;
  | Pcf_virt _ | Pcf_valvirt _ -> ()
  | Pcf_meth (_, _, _, e, _) -> expression ppf tbl e;
  | Pcf_cstr _ -> ()
  | Pcf_let (recflag, pel, _) -> let_pel ppf tbl recflag pel None;
  | Pcf_init e -> expression ppf tbl e;
;;

let warn ppf ast =
  if Warnings.is_active (w_suspicious "") || Warnings.is_active (w_strict "")
  then begin
    let tbl = Hashtbl.create 97 in
    structure ppf tbl ast;
  end;
  ast
;;
