(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Asttypes;;
open Formatmsg;;
open Location;;
open Parsetree;;

let fmt_location f loc =
  if loc.loc_ghost then
    Format.fprintf f "(%d,%d) ghost" loc.loc_start loc.loc_end
  else
    Format.fprintf f "(%d,%d)" loc.loc_start loc.loc_end
;;

let rec fmt_longident_aux f x =
  match x with
  | Longident.Lident (s) -> Format.fprintf f "%s" s;
  | Longident.Ldot (y, s) -> Format.fprintf f "%a.%s" fmt_longident_aux y s;
  | Longident.Lapply (y, z) ->
      Format.fprintf f "%a(%a)" fmt_longident_aux y fmt_longident_aux z;
;;

let fmt_longident f x = Format.fprintf f "\"%a\"" fmt_longident_aux x;;

let fmt_constant f x =
  match x with
  | Const_int (i) -> Format.fprintf f "Const_int %d" i;
  | Const_char (c) -> Format.fprintf f "Const_char %02x" (Char.code c);
  | Const_string (s) -> Format.fprintf f "Const_string \"%s\"" s;
  | Const_float (s) -> Format.fprintf f "Const_float %s" s;
;;

let fmt_mutable_flag f x =
  match x with
  | Immutable -> Format.fprintf f "Immutable";
  | Mutable -> Format.fprintf f "Mutable";
;;

let fmt_virtual_flag f x =
  match x with
  | Virtual -> Format.fprintf f "Virtual";
  | Concrete -> Format.fprintf f "Concrete";
;;

let fmt_rec_flag f x =
  match x with
  | Nonrecursive -> Format.fprintf f "Nonrec";
  | Recursive -> Format.fprintf f "Rec";
  | Default -> Format.fprintf f "Default";
;;

let fmt_direction_flag f x =
  match x with
  | Upto -> Format.fprintf f "Up";
  | Downto -> Format.fprintf f "Down";
;;

let fmt_private_flag f x =
  match x with
  | Public -> Format.fprintf f "Public";
  | Private -> Format.fprintf f "Private";
;;

let line i s (*...*) =
  printf "%s" (String.make (2*i) ' ');
  printf s (*...*)
;;

let list i f l = List.iter (f i) l;;

let option i f x =
  match x with
  | None -> line i "None\n";
  | Some x ->
      line i "Some\n";
      f (i+1) x;
;;

let longident i li = line i "%a" fmt_longident li;;
let string i s = line i "\"%s\"\n" s;;
let bool i x = line i "%s\n" (string_of_bool x);;
let label i x = line i "label=\"%s\"\n" x;;

let rec core_type i x =
  line i "core_type %a\n" fmt_location x.ptyp_loc;
  let i = i+1 in
  match x.ptyp_desc with
  | Ptyp_any -> line i "Ptyp_any\n";
  | Ptyp_var (s) -> line i "Ptyp_var %s\n" s;
  | Ptyp_arrow (l, ct1, ct2) ->
      line i "Ptyp_arrow\n";
      string i l;
      core_type i ct1;
      core_type i ct2;
  | Ptyp_tuple l ->
      line i "Ptyp_tuple\n";
      list i core_type l;
  | Ptyp_constr (li, l) ->
      line i "Ptyp_constr %a\n" fmt_longident li;
      list i core_type l;
  | Ptyp_variant (l, closed, low) ->
      line i "Ptyp_variant closed=%s\n" (string_of_bool closed);
      list i label_x_bool_x_core_type_list l;
      list i string low
  | Ptyp_object (l) ->
      line i "Ptyp_object\n";
      list i core_field_type l;
  | Ptyp_class (li, l, low) ->
      line i "Ptyp_class %a\n" fmt_longident li;
      list i core_type l;
      list i string low
  | Ptyp_alias (ct, s) ->
      line i "Ptyp_alias \"%s\"\n" s;
      core_type i ct;

and core_field_type i x =
  line i "core_field_type %a\n" fmt_location x.pfield_loc;
  let i = i+1 in
  match x.pfield_desc with
  | Pfield (s, ct) ->
      line i "Pfield \"%s\"\n" s;
      core_type i ct;
  | Pfield_var -> line i "Pfield_var\n";

and pattern i x =
  line i "pattern %a\n" fmt_location x.ppat_loc;
  let i = i+1 in
  match x.ppat_desc with
  | Ppat_any -> line i "Ppat_any\n";
  | Ppat_var (s) -> line i "Ppat_var \"%s\"\n" s;
  | Ppat_alias (p, s) ->
      line i "Ppat_alias \"%s\"\n" s;
      pattern i p;
  | Ppat_constant (c) -> line i "Ppat_constant %a\n" fmt_constant c;
  | Ppat_tuple (l) ->
      line i "Ppat_tuple\n";
      list i pattern l;
  | Ppat_construct (li, po, b) ->
      line i "Ppat_construct %a\n" fmt_longident li;
      option i pattern po;
      bool i b;
  | Ppat_variant (l, po) ->
      line i "Ppat_variant `%s\n" l;
      option i pattern po;
  | Ppat_record (l) ->
      line i "Ppat_record\n";
      list i longident_x_pattern l;
  | Ppat_array (l) ->
      line i "Ppat_array\n";
      list i pattern l;
  | Ppat_or (p1, p2) ->
      line i "Ppat_or\n";
      pattern i p1;
      pattern i p2;
  | Ppat_constraint (p, ct) ->
      line i "Ppat_constraint";
      pattern i p;
      core_type i ct;

and expression i x =
  line i "expression %a\n" fmt_location x.pexp_loc;
  let i = i+1 in
  match x.pexp_desc with
  | Pexp_ident (li) -> line i "Pexp_ident %a\n" fmt_longident li;
  | Pexp_constant (c) -> line i "Pexp_constant %a\n" fmt_constant c;
  | Pexp_let (rf, l, e) ->
      line i "Pexp_let %a\n" fmt_rec_flag rf;
      list i pattern_x_expression_def l;
      expression i e;
  | Pexp_function (p, eo, l) ->
      line i "Pexp_function \"%s\"\n" p;
      option i expression eo;
      list i pattern_x_expression_case l;
  | Pexp_apply (e, l) ->
      line i "Pexp_apply\n";
      expression i e;
      list i label_x_expression l;
  | Pexp_match (e, l) ->
      line i "Pexp_match\n";
      expression i e;
      list i pattern_x_expression_case l;
  | Pexp_try (e, l) ->
      line i "Pexp_try\n";
      expression i e;
      list i pattern_x_expression_case l;
  | Pexp_tuple (l) ->
      line i "Pexp_tuple\n";
      list i expression l;
  | Pexp_construct (li, eo, b) ->
      line i "Pexp_construct %a\n" fmt_longident li;
      option i expression eo;
      bool i b;
  | Pexp_variant (l, eo) ->
      line i "Pexp_variant `%s\n" l;
      option i expression eo;
  | Pexp_record (l, eo) ->
      line i "Pexp_record\n";
      list i longident_x_expression l;
      option i expression eo;
  | Pexp_field (e, li) ->
      line i "Pexp_field\n";
      expression i e;
      longident i li;
  | Pexp_setfield (e1, li, e2) ->
      line i "Pexp_setfield\n";
      expression i e1;
      longident i li;
      expression i e2;
  | Pexp_array (l) ->
      line i "Pexp_array\n";
      list i expression l;
  | Pexp_ifthenelse (e1, e2, eo) ->
      line i "Pexp_ifthenelse\n";
      expression i e1;
      expression i e2;
      option i expression eo;
  | Pexp_sequence (e1, e2) ->
      line i "Pexp_sequence\n";
      expression i e1;
      expression i e2;
  | Pexp_while (e1, e2) ->
      line i "Pexp_while\n";
      expression i e1;
      expression i e2;
  | Pexp_for (s, e1, e2, df, e3) ->
      line i "Pexp_for \"%s\" %a\n" s fmt_direction_flag df;
      expression i e1;
      expression i e2;
      expression i e3;
  | Pexp_constraint (e, cto1, cto2) ->
      line i "Pexp_constraint\n";
      expression i e;
      option i core_type cto1;
      option i core_type cto2;
  | Pexp_when (e1, e2) ->
      line i "Pexp_when\n";
      expression i e1;
      expression i e2;
  | Pexp_send (e, s) ->
      line i "Pexp_send \"%s\"\n" s;
      expression i e;
  | Pexp_new (li) -> line i "Pexp_new %a\n" fmt_longident li;
  | Pexp_setinstvar (s, e) ->
      line i "Pexp_setinstvar \"%s\"\n" s;
      expression i e;
  | Pexp_override (l) ->
      line i "Pexp_override\n";
      list i string_x_expression l;
  | Pexp_letmodule (s, me, e) ->
      line i "Pexp_letmodule \"%s\"\n" s;
      module_expr i me;
      expression i e;

and value_description i x =
  line i "value_description\n";
  core_type (i+1) x.pval_type;
  list (i+1) string x.pval_prim;

and type_declaration i x =
  line i "type_declaration %a\n" fmt_location x.ptype_loc;
  let i = i+1 in
  line i "ptype_params =\n";
  list (i+1) string x.ptype_params;
  line i "ptype_cstrs =\n";
  list (i+1) core_type_x_core_type_x_location x.ptype_cstrs;
  line i "ptype_kind =\n";
  type_kind (i+1) x.ptype_kind;
  line i "ptype_manifest =\n";
  option (i+1) core_type x.ptype_manifest;

and type_kind i x =
  match x with
  | Ptype_abstract -> line i "Ptype_abstract\n";
  | Ptype_variant (l) ->
      line i "Ptype_variant\n";
      list (i+1) string_x_core_type_list l;
  | Ptype_record (l) ->
      line i "Ptype_record\n";
      list (i+1) string_x_mutable_flag_x_core_type l;

and exception_declaration i x = list i core_type x

and class_type i x =
  line i "class_type %a\n" fmt_location x.pcty_loc;
  let i = i+1 in
  match x.pcty_desc with
  | Pcty_constr (li, l) ->
      line i "Pcty_constr %a\n" fmt_longident li;
      list i core_type l;
  | Pcty_signature (cs) ->
      line i "Pcty_signature\n";
      class_signature i cs;
  | Pcty_fun (l, co, cl) ->
      line i "Pcty_fun \"%s\"\n" l;
      core_type i co;
      class_type i cl;

and class_signature i (ct, l) =
  line i "class_signature\n";
  core_type (i+1) ct;
  list (i+1) class_type_field l;

and class_type_field i x =
  match x with
  | Pctf_inher (ct) ->
      line i "Pctf_inher\n";
      class_type i ct;
  | Pctf_val (s, mf, cto, loc) ->
      line i "Pctf_val \"%s\" %a %a\n" s fmt_mutable_flag mf fmt_location loc;
      option i core_type cto;
  | Pctf_virt (s, pf, ct, loc) ->
      line i "Pctf_virt \"%s\" %a %a\n" s fmt_private_flag pf fmt_location loc;
  | Pctf_meth (s, pf, ct, loc) ->
      line i "Pctf_meth \"%s\" %a %a\n" s fmt_private_flag pf fmt_location loc;
  | Pctf_cstr (ct1, ct2, loc) ->
      line i "Pctf_cstr %a\n" fmt_location loc;
      core_type i ct1;
      core_type i ct2;

and class_description i x =
  line i "class_description %a\n" fmt_location x.pci_loc;
  let i = i+1 in
  line i "pci_virt = %a\n" fmt_virtual_flag x.pci_virt;
  line i "pci_params =\n";
  string_list_x_location (i+1) x.pci_params;
  line i "pci_name = \"%s\"\n" x.pci_name;
  line i "pci_expr =\n";
  class_type (i+1) x.pci_expr;

and class_type_declaration i x =
  line i "class_type_declaration %a\n" fmt_location x.pci_loc;
  let i = i+1 in
  line i "pci_virt = %a\n" fmt_virtual_flag x.pci_virt;
  line i "pci_params =\n";
  string_list_x_location (i+1) x.pci_params;
  line i "pci_name = \"%s\"\n" x.pci_name;
  line i "pci_expr =\n";
  class_type (i+1) x.pci_expr;

and class_expr i x =
  line i "class_expr %a\n" fmt_location x.pcl_loc;
  let i = i+1 in
  match x.pcl_desc with
  | Pcl_constr (li, l) ->
      line i "Pcl_constr %a\n" fmt_longident li;
      list i core_type l;
  | Pcl_structure (cs) ->
      line i "Pcl_structure\n";
      class_structure i cs;
  | Pcl_fun (l, eo, p, e) ->
      line i "Pcl_fun\n";
      label i l;
      option i expression eo;
      pattern i p;
      class_expr i e;
  | Pcl_apply (ce, l) ->
      line i "Pcl_apply\n";
      class_expr i ce;
      list i label_x_expression l;
  | Pcl_let (rf, l, ce) ->
      line i "Pcl_let %a\n" fmt_rec_flag rf;
      list i pattern_x_expression_def l;
      class_expr i ce;
  | Pcl_constraint (ce, ct) ->
      line i "Pcl_constraint\n";
      class_expr i ce;
      class_type i ct;

and class_structure i (p, l) =
  line i "class_structure\n";
  pattern (i+1) p;
  list (i+1) class_field l;

and class_field i x =
  match x with
  | Pcf_inher (ce, so) ->
      printf "Pcf_inher\n";
      class_expr (i+1) ce;
      option (i+1) string so;
  | Pcf_val (s, mf, e, loc) ->
      line i "Pcf_val \"%s\" %a %a\n" s fmt_mutable_flag mf fmt_location loc;
      expression (i+1) e;
  | Pcf_virt (s, pf, ct, loc) ->
      line i "Pcf_virt \"%s\" %a %a\n" s fmt_private_flag pf fmt_location loc;
      core_type (i+1) ct;
  | Pcf_meth (s, pf, e, loc) ->
      line i "Pcf_meth \"%s\" %a %a\n" s fmt_private_flag pf fmt_location loc;
      expression (i+1) e;
  | Pcf_cstr (ct1, ct2, loc) ->
      line i "Pcf_cstr %a\n" fmt_location loc;
      core_type (i+1) ct1;
      core_type (i+1) ct2;
  | Pcf_let (rf, l, loc) ->
      line i "Pcf_let %a %a\n" fmt_rec_flag rf fmt_location loc;
      list (i+1) pattern_x_expression_def l;
  | Pcf_init (e) ->
      line i "Pcf_init\n";
      expression (i+1) e;

and class_declaration i x =
  line i "class_declaration %a\n" fmt_location x.pci_loc;
  let i = i+1 in
  line i "pci_virt = %a\n" fmt_virtual_flag x.pci_virt;
  line i "pci_params =\n";
  string_list_x_location (i+1) x.pci_params;
  line i "pci_name = \"%s\"\n" x.pci_name;
  line i "pci_expr =\n";
  class_expr (i+1) x.pci_expr;

and module_type i x =
  line i "module_type %a\n" fmt_location x.pmty_loc;
  let i = i+1 in
  match x.pmty_desc with
  | Pmty_ident (li) -> line i "Pmty_ident (%a)\n" fmt_longident li;
  | Pmty_signature (s) ->
      line i "Pmty_signature\n";
      signature i s;
  | Pmty_functor (s, mt1, mt2) ->
      line i "Pmty_functor \"%s\"\n" s;
      module_type i mt1;
      module_type i mt2;
  | Pmty_with (mt, l) ->
      line i "Pmty_with\n";
      module_type i mt;
      list i longident_x_with_constraint l;

and signature i x = list i signature_item x

and signature_item i x =
  line i "signature_item %a\n" fmt_location x.psig_loc;
  let i = i+1 in
  match x.psig_desc with
  | Psig_value (s, vd) ->
      line i "Psig_value \"%s\"\n" s;
      value_description i vd;
  | Psig_type (l) ->
      line i "Psig_type\n";
      list i string_x_type_declaration l;
  | Psig_exception (s, ed) ->
      line i "Psig_exception \"%s\"\n" s;
      exception_declaration i ed;
  | Psig_module (s, mt) ->
      line i "Psig_module \"%s\"\n" s;
      module_type i mt;
  | Psig_modtype (s, md) ->
      line i "Psig_modtype \"%s\"\n" s;
      modtype_declaration i md;
  | Psig_open (li) -> line i "Psig_open %a\n" fmt_longident li;
  | Psig_include (mt) ->
      line i "Psig_include\n";
      module_type i mt;
  | Psig_class (l) ->
      line i "Psig_class\n";
      list i class_description l;
  | Psig_class_type (l) ->
      line i "Psig_class_type\n";
      list i class_type_declaration l;

and modtype_declaration i x =
  match x with
  | Pmodtype_abstract -> line i "Pmodtype_abstract\n";
  | Pmodtype_manifest (mt) ->
      line i "Pmodtype_manifest\n";
      module_type (i+1) mt;

and with_constraint i x =
  match x with
  | Pwith_type (td) ->
      line i "Pwith_type\n";
      type_declaration (i+1) td;
  | Pwith_module (li) -> line i "Pwith_module %a\n" fmt_longident li;

and module_expr i x =
  line i "module_expr %a\n" fmt_location x.pmod_loc;
  let i = i+1 in
  match x.pmod_desc with
  | Pmod_ident (li) -> line i "Pmod_ident %a\n" fmt_longident li;
  | Pmod_structure (s) ->
      line i "Pmod_structure\n";
      structure i s;
  | Pmod_functor (s, mt, me) ->
      line i "Pmod_functor \"%s\"\n" s;
      module_type i mt;
      module_expr i me;
  | Pmod_apply (me1, me2) ->
      line i "Pmod_apply\n";
      module_expr i me1;
      module_expr i me2;
  | Pmod_constraint (me, mt) ->
      line i "Pmod_constraint\n";
      module_expr i me;
      module_type i mt;

and structure i x = list i structure_item x

and structure_item i x =
  line i "structure_item %a\n" fmt_location x.pstr_loc;
  let i = i+1 in
  match x.pstr_desc with
  | Pstr_eval (e) ->
      line i "Pstr_eval\n";
      expression i e;
  | Pstr_value (rf, l) ->
      line i "Pstr_value %a\n" fmt_rec_flag rf;
      list i pattern_x_expression_def l;
  | Pstr_primitive (s, vd) ->
      line i "Pstr_primitive \"%s\"\n" s;
      value_description i vd;
  | Pstr_type (l) ->
      line i "Pstr_type\n";
      list i string_x_type_declaration l;
  | Pstr_exception (s, ed) ->
      line i "Pstr_exception \"%s\"\n" s;
      exception_declaration i ed;
  | Pstr_module (s, me) ->
      line i "Pstr_module \"%s\"\n" s;
      module_expr i me;
  | Pstr_modtype (s, mt) ->
      line i "Pstr_modtype \"%s\"\n" s;
      module_type i mt;
  | Pstr_open (li) -> line i "Pstr_open %a\n" fmt_longident li;
  | Pstr_class (l) ->
      line i "Pstr_class\n";
      list i class_declaration l;
  | Pstr_class_type (l) ->
      line i "Pstr_class_type\n";
      list i class_type_declaration l;

and string_x_type_declaration i (s, td) =
  string i s;
  type_declaration (i+1) td;

and longident_x_with_constraint i (li, wc) =
  line i "%a\n" fmt_longident li;
  with_constraint (i+1) wc;

and core_type_x_core_type_x_location i (ct1, ct2, l) =
  line i "<constraint> %a\n" fmt_location l;
  core_type (i+1) ct1;
  core_type (i+1) ct2;

and string_x_core_type_list i (s, l) =
  string i s;
  list (i+1) core_type l;

and string_x_mutable_flag_x_core_type i (s, mf, ct) =
  line i "\"%s\" %a\n" s fmt_mutable_flag mf;
  core_type (i+1) ct;

and string_list_x_location i (l, loc) =
  line i "<params> %a\n" fmt_location loc;
  list (i+1) string l;

and longident_x_pattern i (li, p) =
  line i "%a\n" fmt_longident li;
  pattern (i+1) p;

and pattern_x_expression_case i (p, e) =
  line i "<case>\n";
  pattern (i+1) p;
  expression (i+1) e;

and pattern_x_expression_def i (p, e) =
  line i "<def>\n";
  pattern (i+1) p;
  expression (i+1) e;

and string_x_expression i (s, e) =
  line i "<override> \"%s\"\n" s;
  expression (i+1) e;

and longident_x_expression i (li, e) =
  line i "%a\n" fmt_longident li;
  expression (i+1) e;

and label_x_expression i (l,e) =
  line i "<label> \"%s\"\n" l;
  expression (i+1) e;

and label_x_bool_x_core_type_list i (l, b, ctl) =
  line i "<row_field> \"%s\" %s\n" l (string_of_bool b);
  list (i+1) core_type ctl
;;

let rec toplevel_phrase i x =
  match x with
  | Ptop_def (s) ->
      line i "Ptop_def\n";
      structure (i+1) s;
  | Ptop_dir (s, da) ->
      line i "Ptop_dir \"%s\"\n" s;
      directive_argument i da;

and directive_argument i x =
  match x with
  | Pdir_none -> line i "Pdir_none\n"
  | Pdir_string (s) -> line i "Pdir_string \"%s\"\n" s;
  | Pdir_int (i) -> line i "Pdir_int %d\n" i;
  | Pdir_ident (li) -> line i "Pdir_ident %a\n" fmt_longident li;
  | Pdir_bool (b) -> line i "Pdir_bool %s\n" (string_of_bool b);
;;

let interface x = list 0 signature_item x;;

let implementation x = list 0 structure_item x;;

let top_phrase x = toplevel_phrase 0 x;;
