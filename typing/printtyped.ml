(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                  Fabrice Le Fessant, INRIA Saclay                   *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Tublic License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Asttypes;;
open Format;;
open Lexing;;
open Location;;
open Typedtree;;

let fmt_position f l =
  if l.pos_lnum = -1
  then fprintf f "%s[%d]" l.pos_fname l.pos_cnum
  else fprintf f "%s[%d,%d+%d]" l.pos_fname l.pos_lnum l.pos_bol
               (l.pos_cnum - l.pos_bol)
;;

let fmt_location f loc =
  fprintf f "(%a..%a)" fmt_position loc.loc_start fmt_position loc.loc_end;
  if loc.loc_ghost then fprintf f " ghost";
;;

let rec fmt_longident_aux f x =
  match x with
  | Longident.Lident (s) -> fprintf f "%s" s;
  | Longident.Ldot (y, s) -> fprintf f "%a.%s" fmt_longident_aux y s;
  | Longident.Lapply (y, z) ->
      fprintf f "%a(%a)" fmt_longident_aux y fmt_longident_aux z;
;;

let fmt_longident_noloc f x = fprintf f "\"%a\"" fmt_longident_aux x;;
let fmt_longident f x = fprintf f "\"%a\"" fmt_longident_aux x.txt;;

let fmt_ident = Ident.print

let rec fmt_path_aux f x =
  match x with
  | Path.Pident (s) -> fprintf f "%a" fmt_ident s;
  | Path.Pdot (y, s, _pos) -> fprintf f "%a.%s" fmt_path_aux y s;
  | Path.Papply (y, z) ->
      fprintf f "%a(%a)" fmt_path_aux y fmt_path_aux z;
;;

let fmt_path f x = fprintf f "\"%a\"" fmt_path_aux x;;
let fmt_path_loc f x = fprintf f "\"%a\"" fmt_path_aux x.txt;;

let fmt_constant f x =
  match x with
  | Const_int (i) -> fprintf f "Const_int %d" i;
  | Const_char (c) -> fprintf f "Const_char %02x" (Char.code c);
  | Const_string (s, None) -> fprintf f "Const_string(%S,None)" s;
  | Const_string (s, Some delim) ->
      fprintf f "Const_string (%S,Some %S)" s delim;
  | Const_float (s) -> fprintf f "Const_float %s" s;
  | Const_int32 (i) -> fprintf f "Const_int32 %ld" i;
  | Const_int64 (i) -> fprintf f "Const_int64 %Ld" i;
  | Const_nativeint (i) -> fprintf f "Const_nativeint %nd" i;
;;

let fmt_mutable_flag f x =
  match x with
  | Immutable -> fprintf f "Immutable";
  | Mutable -> fprintf f "Mutable";
;;

let fmt_virtual_flag f x =
  match x with
  | Virtual -> fprintf f "Virtual";
  | Concrete -> fprintf f "Concrete";
;;

let fmt_override_flag f x =
  match x with
  | Override -> fprintf f "Override";
  | Fresh -> fprintf f "Fresh";
;;

let fmt_closed_flag f x =
  match x with
  | Closed -> fprintf f "Closed"
  | Open -> fprintf f "Open"

let fmt_rec_flag f x =
  match x with
  | Nonrecursive -> fprintf f "Nonrec";
  | Recursive -> fprintf f "Rec";
;;

let fmt_direction_flag f x =
  match x with
  | Upto -> fprintf f "Up";
  | Downto -> fprintf f "Down";
;;

let fmt_private_flag f x =
  match x with
  | Public -> fprintf f "Public";
  | Private -> fprintf f "Private";
;;

let line i f s (*...*) =
  fprintf f "%s" (String.make (2*i) ' ');
  fprintf f s (*...*)
;;

let list i f ppf l =
  match l with
  | [] -> line i ppf "[]\n";
  | _ :: _ ->
     line i ppf "[\n";
     List.iter (f (i+1) ppf) l;
     line i ppf "]\n";
;;

let option i f ppf x =
  match x with
  | None -> line i ppf "None\n";
  | Some x ->
      line i ppf "Some\n";
      f (i+1) ppf x;
;;

let longident i ppf li = line i ppf "%a\n" fmt_longident li;;
let path i ppf li = line i ppf "%a\n" fmt_path li;;
let ident i ppf li = line i ppf "%a\n" fmt_ident li;;
let string i ppf s = line i ppf "\"%s\"\n" s;;
let string_loc i ppf s = line i ppf "\"%s\"\n" s.txt;;
let bool i ppf x = line i ppf "%s\n" (string_of_bool x);;
let label i ppf x = line i ppf "label=\"%s\"\n" x;;

let attributes i ppf l =
  let i = i + 1 in
  List.iter
    (fun (s, arg) ->
      line i ppf "attribute \"%s\"\n" s.txt;
      Printast.payload (i + 1) ppf arg;
    )
    l

let rec core_type i ppf x =
  line i ppf "core_type %a\n" fmt_location x.ctyp_loc;
  attributes i ppf x.ctyp_attributes;
  let i = i+1 in
  match x.ctyp_desc with
  | Ttyp_any -> line i ppf "Ptyp_any\n";
  | Ttyp_var (s) -> line i ppf "Ptyp_var %s\n" s;
  | Ttyp_arrow (l, ct1, ct2) ->
      line i ppf "Ptyp_arrow\n";
      string i ppf l;
      core_type i ppf ct1;
      core_type i ppf ct2;
  | Ttyp_tuple l ->
      line i ppf "Ptyp_tuple\n";
      list i core_type ppf l;
  | Ttyp_constr (li, _, l) ->
      line i ppf "Ptyp_constr %a\n" fmt_path li;
      list i core_type ppf l;
  | Ttyp_variant (l, closed, low) ->
      line i ppf "Ptyp_variant closed=%a\n" fmt_closed_flag closed;
      list i label_x_bool_x_core_type_list ppf l;
      option i (fun i -> list i string) ppf low
  | Ttyp_object (l, c) ->
      line i ppf "Ptyp_object %a\n" fmt_closed_flag c;
      let i = i + 1 in
      List.iter
        (fun (s, t) ->
          line i ppf "method %s" s;
          core_type (i + 1) ppf t
        )
        l
  | Ttyp_class (li, _, l) ->
      line i ppf "Ptyp_class %a\n" fmt_path li;
      list i core_type ppf l;
  | Ttyp_alias (ct, s) ->
      line i ppf "Ptyp_alias \"%s\"\n" s;
      core_type i ppf ct;
  | Ttyp_poly (sl, ct) ->
      line i ppf "Ptyp_poly%a\n"
        (fun ppf -> List.iter (fun x -> fprintf ppf " '%s" x)) sl;
      core_type i ppf ct;
  | Ttyp_package { pack_name = s; pack_fields = l } ->
      line i ppf "Ptyp_package %a\n" fmt_path s;
      list i package_with ppf l;

and package_with i ppf (s, t) =
  line i ppf "with type %a\n" fmt_longident s;
  core_type i ppf t

and pattern i ppf x =
  line i ppf "pattern %a\n" fmt_location x.pat_loc;
  attributes i ppf x.pat_attributes;
  let i = i+1 in
  match x.pat_extra with
    | (Tpat_unpack, _, attrs) :: rem ->
        line i ppf "Tpat_unpack\n";
        attributes i ppf attrs;
        pattern i ppf { x with pat_extra = rem }
    | (Tpat_constraint cty, _, attrs) :: rem ->
        line i ppf "Tpat_constraint\n";
        attributes i ppf attrs;
        core_type i ppf cty;
        pattern i ppf { x with pat_extra = rem }
    | (Tpat_type (id, _), _, attrs) :: rem ->
        line i ppf "Tpat_type %a\n" fmt_path id;
        attributes i ppf attrs;
        pattern i ppf { x with pat_extra = rem }
    | [] ->
  match x.pat_desc with
  | Tpat_any -> line i ppf "Ppat_any\n";
  | Tpat_var (s,_) -> line i ppf "Ppat_var \"%a\"\n" fmt_ident s;
  | Tpat_alias (p, s,_) ->
      line i ppf "Ppat_alias \"%a\"\n" fmt_ident s;
      pattern i ppf p;
  | Tpat_constant (c) -> line i ppf "Ppat_constant %a\n" fmt_constant c;
  | Tpat_tuple (l) ->
      line i ppf "Ppat_tuple\n";
      list i pattern ppf l;
  | Tpat_construct (li, _, po) ->
      line i ppf "Ppat_construct %a\n" fmt_longident li;
      list i pattern ppf po;
  | Tpat_variant (l, po, _) ->
      line i ppf "Ppat_variant \"%s\"\n" l;
      option i pattern ppf po;
  | Tpat_record (l, c) ->
      line i ppf "Ppat_record\n";
      list i longident_x_pattern ppf l;
  | Tpat_array (l) ->
      line i ppf "Ppat_array\n";
      list i pattern ppf l;
  | Tpat_or (p1, p2, _) ->
      line i ppf "Ppat_or\n";
      pattern i ppf p1;
      pattern i ppf p2;
  | Tpat_lazy p ->
      line i ppf "Ppat_lazy\n";
      pattern i ppf p;

and expression_extra i ppf x attrs =
  match x with
  | Texp_constraint ct ->
      line i ppf "Pexp_constraint\n";
      attributes i ppf attrs;
      core_type i ppf ct;
  | Texp_coerce (cto1, cto2) ->
      line i ppf "Pexp_constraint\n";
      attributes i ppf attrs;
      option i core_type ppf cto1;
      core_type i ppf cto2;
  | Texp_open (ovf, m, _, _) ->
      line i ppf "Pexp_open %a \"%a\"\n" fmt_override_flag ovf fmt_path m;
      attributes i ppf attrs;
  | Texp_poly cto ->
      line i ppf "Pexp_poly\n";
      attributes i ppf attrs;
      option i core_type ppf cto;
  | Texp_newtype s ->
      line i ppf "Pexp_newtype \"%s\"\n" s;
      attributes i ppf attrs;

and expression i ppf x =
  line i ppf "expression %a\n" fmt_location x.exp_loc;
  attributes i ppf x.exp_attributes;
  let i =
    List.fold_left (fun i (extra,_,attrs) -> expression_extra i ppf extra attrs; i+1)
      (i+1) x.exp_extra
  in
  match x.exp_desc with
  | Texp_ident (li,_,_) -> line i ppf "Pexp_ident %a\n" fmt_path li;
  | Texp_instvar (_, li,_) -> line i ppf "Pexp_instvar %a\n" fmt_path li;
  | Texp_constant (c) -> line i ppf "Pexp_constant %a\n" fmt_constant c;
  | Texp_let (rf, l, e) ->
      line i ppf "Pexp_let %a\n" fmt_rec_flag rf;
      list i value_binding ppf l;
      expression i ppf e;
  | Texp_function (p, l, _partial) ->
      line i ppf "Pexp_function \"%s\"\n" p;
(*      option i expression ppf eo; *)
      list i case ppf l;
  | Texp_apply (e, l) ->
      line i ppf "Pexp_apply\n";
      expression i ppf e;
      list i label_x_expression ppf l;
  | Texp_match (e, l, partial) ->
      line i ppf "Pexp_match\n";
      expression i ppf e;
      list i case ppf l;
  | Texp_try (e, l) ->
      line i ppf "Pexp_try\n";
      expression i ppf e;
      list i case ppf l;
  | Texp_tuple (l) ->
      line i ppf "Pexp_tuple\n";
      list i expression ppf l;
  | Texp_construct (li, _, eo) ->
      line i ppf "Pexp_construct %a\n" fmt_longident li;
      list i expression ppf eo;
  | Texp_variant (l, eo) ->
      line i ppf "Pexp_variant \"%s\"\n" l;
      option i expression ppf eo;
  | Texp_record (l, eo) ->
      line i ppf "Pexp_record\n";
      list i longident_x_expression ppf l;
      option i expression ppf eo;
  | Texp_field (e, li, _) ->
      line i ppf "Pexp_field\n";
      expression i ppf e;
      longident i ppf li;
  | Texp_setfield (e1, li, _, e2) ->
      line i ppf "Pexp_setfield\n";
      expression i ppf e1;
      longident i ppf li;
      expression i ppf e2;
  | Texp_array (l) ->
      line i ppf "Pexp_array\n";
      list i expression ppf l;
  | Texp_ifthenelse (e1, e2, eo) ->
      line i ppf "Pexp_ifthenelse\n";
      expression i ppf e1;
      expression i ppf e2;
      option i expression ppf eo;
  | Texp_sequence (e1, e2) ->
      line i ppf "Pexp_sequence\n";
      expression i ppf e1;
      expression i ppf e2;
  | Texp_while (e1, e2) ->
      line i ppf "Pexp_while\n";
      expression i ppf e1;
      expression i ppf e2;
  | Texp_for (s, _, e1, e2, df, e3) ->
      line i ppf "Pexp_for \"%a\" %a\n" fmt_ident s fmt_direction_flag df;
      expression i ppf e1;
      expression i ppf e2;
      expression i ppf e3;
  | Texp_send (e, Tmeth_name s, eo) ->
      line i ppf "Pexp_send \"%s\"\n" s;
      expression i ppf e;
      option i expression ppf eo
  | Texp_send (e, Tmeth_val s, eo) ->
      line i ppf "Pexp_send \"%a\"\n" fmt_ident s;
      expression i ppf e;
      option i expression ppf eo
  | Texp_new (li, _, _) -> line i ppf "Pexp_new %a\n" fmt_path li;
  | Texp_setinstvar (_, s, _, e) ->
      line i ppf "Pexp_setinstvar \"%a\"\n" fmt_path s;
      expression i ppf e;
  | Texp_override (_, l) ->
      line i ppf "Pexp_override\n";
      list i string_x_expression ppf l;
  | Texp_letmodule (s, _, me, e) ->
      line i ppf "Pexp_letmodule \"%a\"\n" fmt_ident s;
      module_expr i ppf me;
      expression i ppf e;
  | Texp_assert (e) ->
      line i ppf "Pexp_assert";
      expression i ppf e;
  | Texp_lazy (e) ->
      line i ppf "Pexp_lazy";
      expression i ppf e;
  | Texp_object (s, _) ->
      line i ppf "Pexp_object";
      class_structure i ppf s
  | Texp_pack me ->
      line i ppf "Pexp_pack";
      module_expr i ppf me

and value_description i ppf x =
  line i ppf "value_description %a %a\n" fmt_ident x.val_id fmt_location x.val_loc;
  attributes i ppf x.val_attributes;
  core_type (i+1) ppf x.val_desc;
  list (i+1) string ppf x.val_prim;

and type_parameter i ppf (x, _variance) =
  match x with
  | Some x ->
      string i ppf x.txt
  | None ->
      string i ppf "_"

and type_declaration i ppf x =
  line i ppf "type_declaration %a %a\n" fmt_ident x.typ_id fmt_location x.typ_loc;
  attributes i ppf x.typ_attributes;
  let i = i+1 in
  line i ppf "ptype_params =\n";
  list (i+1) type_parameter ppf x.typ_params;
  line i ppf "ptype_cstrs =\n";
  list (i+1) core_type_x_core_type_x_location ppf x.typ_cstrs;
  line i ppf "ptype_kind =\n";
  type_kind (i+1) ppf x.typ_kind;
  line i ppf "ptype_private = %a\n" fmt_private_flag x.typ_private;
  line i ppf "ptype_manifest =\n";
  option (i+1) core_type ppf x.typ_manifest;

and type_kind i ppf x =
  match x with
  | Ttype_abstract ->
      line i ppf "Ptype_abstract\n"
  | Ttype_variant l ->
      line i ppf "Ptype_variant\n";
      list (i+1) constructor_decl ppf l;
  | Ttype_record l ->
      line i ppf "Ptype_record\n";
      list (i+1) label_decl ppf l;

and class_type i ppf x =
  line i ppf "class_type %a\n" fmt_location x.cltyp_loc;
  attributes i ppf x.cltyp_attributes;
  let i = i+1 in
  match x.cltyp_desc with
  | Tcty_constr (li, _, l) ->
      line i ppf "Pcty_constr %a\n" fmt_path li;
      list i core_type ppf l;
  | Tcty_signature (cs) ->
      line i ppf "Pcty_signature\n";
      class_signature i ppf cs;
  | Tcty_arrow (l, co, cl) ->
      line i ppf "Pcty_arrow \"%s\"\n" l;
      core_type i ppf co;
      class_type i ppf cl;

and class_signature i ppf { csig_self = ct; csig_fields = l } =
  line i ppf "class_signature\n";
  core_type (i+1) ppf ct;
  list (i+1) class_type_field ppf l;

and class_type_field i ppf x =
  line i ppf "class_type_field %a\n" fmt_location x.ctf_loc;
  let i = i+1 in
  attributes i ppf x.ctf_attributes;
  match x.ctf_desc with
  | Tctf_inherit (ct) ->
      line i ppf "Pctf_inherit\n";
      class_type i ppf ct;
  | Tctf_val (s, mf, vf, ct) ->
      line i ppf "Pctf_val \"%s\" %a %a\n" s fmt_mutable_flag mf
           fmt_virtual_flag vf;
      core_type (i+1) ppf ct;
  | Tctf_method (s, pf, vf, ct) ->
      line i ppf "Pctf_method \"%s\" %a %a\n" s fmt_private_flag pf fmt_virtual_flag vf;
      core_type (i+1) ppf ct;
  | Tctf_constraint (ct1, ct2) ->
      line i ppf "Pctf_constraint\n";
      core_type (i+1) ppf ct1;
      core_type (i+1) ppf ct2;

and class_description i ppf x =
  line i ppf "class_description %a\n" fmt_location x.ci_loc;
  attributes i ppf x.ci_attributes;
  let i = i+1 in
  line i ppf "pci_virt = %a\n" fmt_virtual_flag x.ci_virt;
  line i ppf "pci_params =\n";
  cl_type_parameters (i+1) ppf x.ci_params;
  line i ppf "pci_name = \"%s\"\n" x.ci_id_name.txt;
  line i ppf "pci_expr =\n";
  class_type (i+1) ppf x.ci_expr;

and class_type_declaration i ppf x =
  line i ppf "class_type_declaration %a\n" fmt_location x.ci_loc;
  let i = i+1 in
  line i ppf "pci_virt = %a\n" fmt_virtual_flag x.ci_virt;
  line i ppf "pci_params =\n";
  cl_type_parameters (i+1) ppf x.ci_params;
  line i ppf "pci_name = \"%s\"\n" x.ci_id_name.txt;
  line i ppf "pci_expr =\n";
  class_type (i+1) ppf x.ci_expr;

and class_expr i ppf x =
  line i ppf "class_expr %a\n" fmt_location x.cl_loc;
  attributes i ppf x.cl_attributes;
  let i = i+1 in
  match x.cl_desc with
  | Tcl_ident (li, _, l) ->
      line i ppf "Pcl_constr %a\n" fmt_path li;
      list i core_type ppf l;
  | Tcl_structure (cs) ->
      line i ppf "Pcl_structure\n";
      class_structure i ppf cs;
  | Tcl_fun (l, eo, p, e, _) -> assert false (* TODO *)
(*      line i ppf "Pcl_fun\n";
      label i ppf l;
      option i expression ppf eo;
      pattern i ppf p;
      class_expr i ppf e; *)
  | Tcl_apply (ce, l) ->
      line i ppf "Pcl_apply\n";
      class_expr i ppf ce;
      list i label_x_expression ppf l;
  | Tcl_let (rf, l1, l2, ce) ->
      line i ppf "Pcl_let %a\n" fmt_rec_flag rf;
      list i value_binding ppf l1;
      list i ident_x_loc_x_expression_def ppf l2;
      class_expr i ppf ce;
  | Tcl_constraint (ce, Some ct, _, _, _) ->
      line i ppf "Pcl_constraint\n";
      class_expr i ppf ce;
      class_type i ppf ct;
  | Tcl_constraint (_, None, _, _, _) -> assert false
        (* TODO : is it possible ? see parsetree *)

and class_structure i ppf { cstr_self = p; cstr_fields = l } =
  line i ppf "class_structure\n";
  pattern (i+1) ppf p;
  list (i+1) class_field ppf l;

and class_field i ppf x = assert false (* TODO *)
(*  let loc = x.cf_loc in
  match x.cf_desc with
  | Tcf_inher (ovf, ce, so) ->
      line i ppf "Pcf_inher %a\n" fmt_override_flag ovf;
      class_expr (i+1) ppf ce;
      option (i+1) string ppf so;
  | Tcf_valvirt (s, mf, ct) ->
      line i ppf "Pcf_valvirt \"%s\" %a %a\n"
        s.txt fmt_mutable_flag mf fmt_location loc;
      core_type (i+1) ppf ct;
  | Tcf_val (s, mf, ovf, e) ->
      line i ppf "Pcf_val \"%s\" %a %a %a\n"
        s.txt fmt_mutable_flag mf fmt_override_flag ovf fmt_location loc;
      expression (i+1) ppf e;
  | Tcf_virt (s, pf, ct) ->
      line i ppf "Pcf_virt \"%s\" %a %a\n"
        s.txt fmt_private_flag pf fmt_location loc;
      core_type (i+1) ppf ct;
  | Tcf_meth (s, pf, ovf, e) ->
      line i ppf "Pcf_meth \"%s\" %a %a %a\n"
        s.txt fmt_private_flag pf fmt_override_flag ovf fmt_location loc;
      expression (i+1) ppf e;
  | Tcf_constr (ct1, ct2) ->
      line i ppf "Pcf_constr %a\n" fmt_location loc;
      core_type (i+1) ppf ct1;
      core_type (i+1) ppf ct2;
  | Tcf_init (e) ->
      line i ppf "Pcf_init\n";
      expression (i+1) ppf e;
*)

and class_declaration i ppf x =
  line i ppf "class_declaration %a\n" fmt_location x.ci_loc;
  let i = i+1 in
  line i ppf "pci_virt = %a\n" fmt_virtual_flag x.ci_virt;
  line i ppf "pci_params =\n";
  cl_type_parameters (i+1) ppf x.ci_params;
  line i ppf "pci_name = \"%s\"\n" x.ci_id_name.txt;
  line i ppf "pci_expr =\n";
  class_expr (i+1) ppf x.ci_expr;

and module_type i ppf x =
  line i ppf "module_type %a\n" fmt_location x.mty_loc;
  attributes i ppf x.mty_attributes;
  let i = i+1 in
  match x.mty_desc with
  | Tmty_ident (li,_) -> line i ppf "Pmty_ident %a\n" fmt_path li;
  | Tmty_signature (s) ->
      line i ppf "Pmty_signature\n";
      signature i ppf s;
  | Tmty_functor (s, _, mt1, mt2) ->
      line i ppf "Pmty_functor \"%a\"\n" fmt_ident s;
      module_type i ppf mt1;
      module_type i ppf mt2;
  | Tmty_with (mt, l) ->
      line i ppf "Pmty_with\n";
      module_type i ppf mt;
      list i longident_x_with_constraint ppf l;
  | Tmty_typeof m ->
      line i ppf "Pmty_typeof\n";
      module_expr i ppf m;

and signature i ppf x = list i signature_item ppf x.sig_items

and signature_item i ppf x =
  line i ppf "signature_item %a\n" fmt_location x.sig_loc;
  let i = i+1 in
  match x.sig_desc with
  | Tsig_value vd ->
      line i ppf "Psig_value\n";
      value_description i ppf vd;
  | Tsig_type l ->
      line i ppf "Psig_type\n";
      list i type_declaration ppf l;
  | Tsig_exception cd ->
      line i ppf "Psig_exception\n";
      constructor_decl i ppf cd
  | Tsig_module md ->
      line i ppf "Psig_module \"%a\"\n" fmt_ident md.md_id;
      attributes i ppf md.md_attributes;
      module_type i ppf md.md_type
  | Tsig_recmodule decls ->
      line i ppf "Psig_recmodule\n";
      list i module_declaration ppf decls;
  | Tsig_modtype x ->
      line i ppf "Psig_modtype \"%a\"\n" fmt_ident x.mtd_id;
      attributes i ppf x.mtd_attributes;
      modtype_declaration i ppf x.mtd_type
  | Tsig_open (ovf, li,_,attrs) ->
      line i ppf "Psig_open %a %a\n" fmt_override_flag ovf  fmt_path li;
      attributes i ppf attrs
  | Tsig_include (mt, _, attrs) ->
      line i ppf "Psig_include\n";
      attributes i ppf attrs;
      module_type i ppf mt
  | Tsig_class (l) ->
      line i ppf "Psig_class\n";
      list i class_description ppf l;
  | Tsig_class_type (l) ->
      line i ppf "Psig_class_type\n";
      list i class_type_declaration ppf l;
  | Tsig_attribute (s, arg) ->
      line i ppf "Psig_attribute \"%s\"\n" s.txt;
      Printast.payload i ppf arg

and module_declaration i ppf md =
  line i ppf "%a" fmt_ident md.md_id;
  attributes i ppf md.md_attributes;
  module_type (i+1) ppf md.md_type;

and module_binding i ppf x =
  line i ppf "%a\n" fmt_ident x.mb_id;
  attributes i ppf x.mb_attributes;
  module_expr (i+1) ppf x.mb_expr

and modtype_declaration i ppf = function
  | None -> line i ppf "#abstract"
  | Some mt -> module_type (i + 1) ppf mt

and with_constraint i ppf x =
  match x with
  | Twith_type (td) ->
      line i ppf "Pwith_type\n";
      type_declaration (i+1) ppf td;
  | Twith_typesubst (td) ->
      line i ppf "Pwith_typesubst\n";
      type_declaration (i+1) ppf td;
  | Twith_module (li,_) -> line i ppf "Pwith_module %a\n" fmt_path li;
  | Twith_modsubst (li,_) -> line i ppf "Pwith_modsubst %a\n" fmt_path li;

and module_expr i ppf x =
  line i ppf "module_expr %a\n" fmt_location x.mod_loc;
  attributes i ppf x.mod_attributes;
  let i = i+1 in
  match x.mod_desc with
  | Tmod_ident (li,_) -> line i ppf "Pmod_ident %a\n" fmt_path li;
  | Tmod_structure (s) ->
      line i ppf "Pmod_structure\n";
      structure i ppf s;
  | Tmod_functor (s, _, mt, me) ->
      line i ppf "Pmod_functor \"%a\"\n" fmt_ident s;
      module_type i ppf mt;
      module_expr i ppf me;
  | Tmod_apply (me1, me2, _) ->
      line i ppf "Pmod_apply\n";
      module_expr i ppf me1;
      module_expr i ppf me2;
  | Tmod_constraint (me, _, Tmodtype_explicit mt, _) ->
      line i ppf "Pmod_constraint\n";
      module_expr i ppf me;
      module_type i ppf mt;
  | Tmod_constraint (me, _, Tmodtype_implicit, _) -> assert false (* TODO *)
(*      line i ppf "Pmod_constraint\n";
      module_expr i ppf me;
      module_type i ppf mt; *)
  | Tmod_unpack (e, _) ->
      line i ppf "Pmod_unpack\n";
      expression i ppf e;

and structure i ppf x = list i structure_item ppf x.str_items

and structure_item i ppf x =
  line i ppf "structure_item %a\n" fmt_location x.str_loc;
  let i = i+1 in
  match x.str_desc with
  | Tstr_eval (e, attrs) ->
      line i ppf "Pstr_eval\n";
      attributes i ppf attrs;
      expression i ppf e;
  | Tstr_value (rf, l) ->
      line i ppf "Pstr_value %a\n" fmt_rec_flag rf;
      list i value_binding ppf l;
  | Tstr_primitive vd ->
      line i ppf "Pstr_primitive\n";
      value_description i ppf vd;
  | Tstr_type l ->
      line i ppf "Pstr_type\n";
      list i type_declaration ppf l;
  | Tstr_exception cd ->
      line i ppf "Pstr_exception\n";
      constructor_decl i ppf cd;
  | Tstr_exn_rebind (s, _, li, _, attrs) ->
      line i ppf "Pstr_exn_rebind \"%a\" %a\n" fmt_ident s fmt_path li;
      attributes i ppf attrs
  | Tstr_module x ->
      line i ppf "Pstr_module\n";
      module_binding i ppf x
  | Tstr_recmodule bindings ->
      line i ppf "Pstr_recmodule\n";
      list i module_binding ppf bindings
  | Tstr_modtype x ->
      line i ppf "Pstr_modtype \"%a\"\n" fmt_ident x.mtd_id;
      attributes i ppf x.mtd_attributes;
      modtype_declaration i ppf x.mtd_type
  | Tstr_open (ovf, li, _, attrs) ->
      line i ppf "Pstr_open %a %a\n" fmt_override_flag ovf fmt_path li;
      attributes i ppf attrs
  | Tstr_class (l) ->
      line i ppf "Pstr_class\n";
      list i class_declaration ppf (List.map (fun (cl, _,_) -> cl) l);
  | Tstr_class_type (l) ->
      line i ppf "Pstr_class_type\n";
      list i class_type_declaration ppf (List.map (fun (_, _, cl) -> cl) l);
  | Tstr_include (me, _, attrs) ->
      line i ppf "Pstr_include";
      attributes i ppf attrs;
      module_expr i ppf me;
  | Tstr_attribute (s, arg) ->
      line i ppf "Pstr_attribute \"%s\"\n" s.txt;
      Printast.payload i ppf arg

and string_x_module_type i ppf (s, _, mty) =
  ident i ppf s;
  module_type (i+1) ppf mty;

and string_x_modtype_x_module i ppf (s, _, mty, modl) =
  ident i ppf s;
  module_type (i+1) ppf mty;
  module_expr (i+1) ppf modl;

and longident_x_with_constraint i ppf (li, _, wc) =
  line i ppf "%a\n" fmt_path li;
  with_constraint (i+1) ppf wc;

and core_type_x_core_type_x_location i ppf (ct1, ct2, l) =
  line i ppf "<constraint> %a\n" fmt_location l;
  core_type (i+1) ppf ct1;
  core_type (i+1) ppf ct2;

and constructor_decl i ppf {cd_id; cd_name = _; cd_args; cd_res; cd_loc; cd_attributes} =
  line i ppf "%a\n" fmt_location cd_loc;
  attributes i ppf cd_attributes;
  line (i+1) ppf "%a\n" fmt_ident cd_id;
  list (i+1) core_type ppf cd_args;
  option (i+1) core_type ppf cd_res

and label_decl i ppf {ld_id; ld_name = _; ld_mutable; ld_type; ld_loc; ld_attributes} =
  line i ppf "%a\n" fmt_location ld_loc;
  attributes i ppf ld_attributes;
  line (i+1) ppf "%a\n" fmt_mutable_flag ld_mutable;
  line (i+1) ppf "%a" fmt_ident ld_id;
  core_type (i+1) ppf ld_type

and cl_type_parameters i ppf l =
  line i ppf "<params>\n";
  list (i+1) cl_type_parameter ppf l;

and cl_type_parameter i ppf (x, _variance) =
  string_loc i ppf x

and longident_x_pattern i ppf (li, _, p) =
  line i ppf "%a\n" fmt_longident li;
  pattern (i+1) ppf p;

and case i ppf {c_lhs; c_guard; c_rhs} =
  line i ppf "<case>\n";
  pattern (i+1) ppf c_lhs;
  begin match c_guard with
  | None -> ()
  | Some g -> line (i+1) ppf "<when>\n"; expression (i + 2) ppf g
  end;
  expression (i+1) ppf c_rhs;

and value_binding i ppf x =
  line i ppf "<def>\n";
  attributes (i+1) ppf x.vb_attributes;
  pattern (i+1) ppf x.vb_pat;
  expression (i+1) ppf x.vb_expr

and string_x_expression i ppf (s, _, e) =
  line i ppf "<override> \"%a\"\n" fmt_path s;
  expression (i+1) ppf e;

and longident_x_expression i ppf (li, _, e) =
  line i ppf "%a\n" fmt_longident li;
  expression (i+1) ppf e;

and label_x_expression i ppf (l, e, _) =
  line i ppf "<label> \"%s\"\n" l;
  (match e with None -> () | Some e -> expression (i+1) ppf e)

and ident_x_loc_x_expression_def i ppf (l,_, e) =
  line i ppf "<def> \"%a\"\n" fmt_ident l;
  expression (i+1) ppf e;

and label_x_bool_x_core_type_list i ppf x =
  match x with
    Ttag (l, b, ctl) ->
      line i ppf "Rtag \"%s\" %s\n" l (string_of_bool b);
      list (i+1) core_type ppf ctl
  | Tinherit (ct) ->
      line i ppf "Rinherit\n";
      core_type (i+1) ppf ct
;;

let interface ppf x = list 0 signature_item ppf x.sig_items;;

let implementation ppf x = list 0 structure_item ppf x.str_items;;

let implementation_with_coercion ppf (x, _) = implementation ppf x
