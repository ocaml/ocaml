(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Location
open Longident
open Parsetree

module StringSet = Set.Make(struct type t = string let compare = compare end)

(* Collect free module identifiers in the a.s.t. *)

let free_structure_names = ref StringSet.empty

let rec addmodule bv lid =
  match lid with
    Lident s ->
      if not (StringSet.mem s bv)
      then free_structure_names := StringSet.add s !free_structure_names
  | Ldot(l, s) -> addmodule bv l
  | Lapply(l1, l2) -> addmodule bv l1; addmodule bv l2

let add bv lid =
  match lid with
    Ldot(l, s) -> addmodule bv l
  | _ -> ()

let rec add_type bv ty =
  match ty.ptyp_desc with
    Ptyp_any -> ()
  | Ptyp_var v -> ()
  | Ptyp_arrow(t1, t2) -> add_type bv t1; add_type bv t2
  | Ptyp_tuple tl -> List.iter (add_type bv) tl
  | Ptyp_constr(c, tl) -> add bv c; List.iter (add_type bv) tl
  | Ptyp_object fl -> List.iter (add_field_type bv) fl
  | Ptyp_class(c, tl) -> add bv c; List.iter (add_type bv) tl
  | Ptyp_alias(t, s) -> add_type bv t

and add_field_type bv ft =
  match ft.pfield_desc with
    Pfield(name, ty) -> add_type bv ty
  | Pfield_var -> ()

let add_opt add_fn bv = function
    None -> ()
  | Some x -> add_fn bv x

let add_type_declaration bv td =
  List.iter
    (fun (ty1, ty2, _) -> add_type bv ty1; add_type bv ty2)
    td.ptype_cstrs;
  add_opt add_type bv td.ptype_manifest;
  match td.ptype_kind with
    Ptype_abstract -> ()
  | Ptype_variant cstrs ->
      List.iter (fun (c, args) -> List.iter (add_type bv) args) cstrs
  | Ptype_record lbls ->
      List.iter (fun (l, mut, ty) -> add_type bv ty) lbls

let rec add_class_type bv cty =
  match cty.pcty_desc with
    Pcty_constr(l, tyl) ->
      add bv l; List.iter (add_type bv) tyl
  | Pcty_signature (ty, fieldl) ->
      add_type bv ty;
      List.iter (add_class_type_field bv) fieldl
  | Pcty_fun(ty1, cty2) ->
      add_type bv ty1; add_class_type bv cty2

and add_class_type_field bv = function
    Pctf_inher cty -> add_class_type bv cty
  | Pctf_val(_, _, oty, _) -> add_opt add_type bv oty
  | Pctf_virt(_, _, ty, _) -> add_type bv ty
  | Pctf_meth(_, _, ty, _) -> add_type bv ty
  | Pctf_cstr(ty1, ty2, _) -> add_type bv ty1; add_type bv ty2

let add_class_description bv infos =
  add_class_type bv infos.pci_expr

let add_class_type_declaration = add_class_description

let rec add_pattern bv pat =
  match pat.ppat_desc with
    Ppat_any -> ()
  | Ppat_var _ -> ()
  | Ppat_alias(p, _) -> add_pattern bv p
  | Ppat_constant _ -> ()
  | Ppat_tuple pl -> List.iter (add_pattern bv) pl
  | Ppat_construct(c, op, _) -> add bv c; add_opt add_pattern bv op
  | Ppat_record pl -> List.iter (fun (_, p) -> add_pattern bv p) pl
  | Ppat_array pl -> List.iter (add_pattern bv) pl
  | Ppat_or(p1, p2) -> add_pattern bv p1; add_pattern bv p2
  | Ppat_constraint(p, ty) -> add_pattern bv p; add_type bv ty

let rec add_expr bv exp =
  match exp.pexp_desc with
    Pexp_ident l -> add bv l
  | Pexp_constant _ -> ()
  | Pexp_let(_, pel, e) -> add_pat_expr_list bv pel; add_expr bv e
  | Pexp_function pel -> add_pat_expr_list bv pel
  | Pexp_apply(e, el) -> add_expr bv e; List.iter (add_expr bv) el
  | Pexp_match(e, pel) -> add_expr bv e; add_pat_expr_list bv pel
  | Pexp_try(e, pel) -> add_expr bv e; add_pat_expr_list bv pel
  | Pexp_tuple el -> List.iter (add_expr bv) el
  | Pexp_construct(c, opte, _) -> add bv c; add_opt add_expr bv opte
  | Pexp_record(lblel, opte) ->
      List.iter (fun (lbl, e) -> add_expr bv e) lblel;
      add_opt add_expr bv opte
  | Pexp_field(e, fld) -> add_expr bv e; add bv fld
  | Pexp_setfield(e1, fld, e2) -> add_expr bv e1; add bv fld; add_expr bv e2
  | Pexp_array el -> List.iter (add_expr bv) el
  | Pexp_ifthenelse(e1, e2, opte3) ->
      add_expr bv e1; add_expr bv e2; add_opt add_expr bv opte3
  | Pexp_sequence(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexp_while(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexp_for(_, e1, e2, _, e3) ->
      add_expr bv e1; add_expr bv e2; add_expr bv e3
  | Pexp_constraint(e1, oty2, oty3) ->
      add_expr bv e1;
      add_opt add_type bv oty2;
      add_opt add_type bv oty3
  | Pexp_when(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexp_send(e, m) -> add_expr bv e
  | Pexp_new l -> add bv l
  | Pexp_setinstvar(v, e) -> add_expr bv e
  | Pexp_override sel -> List.iter (fun (s, e) -> add_expr bv e) sel
  | Pexp_letmodule(id, m, e) ->
      add_module bv m; add_expr (StringSet.add id bv) e

and add_pat_expr_list bv pel =
  List.iter (fun (p, e) -> add_pattern bv p; add_expr bv e) pel

and add_modtype bv mty =
  match mty.pmty_desc with
    Pmty_ident l -> add bv l
  | Pmty_signature s -> add_signature bv s
  | Pmty_functor(id, mty1, mty2) ->
      add_modtype bv mty1; add_modtype (StringSet.add id bv) mty2
  | Pmty_with(mty, cstrl) ->
      add_modtype bv mty;
      List.iter
        (function (_, Pwith_type td) -> add_type_declaration bv td
                | (_, Pwith_module lid) -> addmodule bv lid)
        cstrl

and add_signature bv = function
    [] -> ()
  | item :: rem -> add_signature (add_sig_item bv item) rem

and add_sig_item bv item =
  match item.psig_desc with
    Psig_value(id, vd) ->
      add_type bv vd.pval_type; bv
  | Psig_type dcls ->
      List.iter (fun (id, td) -> add_type_declaration bv td) dcls; bv
  | Psig_exception(id, args) ->
      List.iter (add_type bv) args; bv
  | Psig_module(id, mty) ->
      add_modtype bv mty; StringSet.add id bv
  | Psig_modtype(id, mtyd) ->
      begin match mtyd with
        Pmodtype_abstract -> ()
      | Pmodtype_manifest mty -> add_modtype bv mty
      end;
      bv
  | Psig_open lid ->
      addmodule bv lid; bv
  | Psig_include mty ->
      add_modtype bv mty; bv
  | Psig_class cdl ->
      List.iter (add_class_description bv) cdl; bv
  | Psig_class_type cdtl ->
      List.iter (add_class_type_declaration bv) cdtl; bv

and add_module bv modl =
  match modl.pmod_desc with
    Pmod_ident l -> addmodule bv l
  | Pmod_structure s -> add_structure bv s
  | Pmod_functor(id, mty, modl) ->
      add_modtype bv mty;
      add_module (StringSet.add id bv) modl
  | Pmod_apply(mod1, mod2) ->
      add_module bv mod1; add_module bv mod2
  | Pmod_constraint(modl, mty) ->
      add_module bv modl; add_modtype bv mty

and add_structure bv = function
    [] -> ()
  | item :: rem -> add_structure (add_struct_item bv item) rem

and add_struct_item bv item =
  match item.pstr_desc with
    Pstr_eval e ->
      add_expr bv e; bv
  | Pstr_value(id, pel) ->
      add_pat_expr_list bv pel; bv
  | Pstr_primitive(id, vd) ->
      add_type bv vd.pval_type; bv
  | Pstr_type dcls ->
      List.iter (fun (id, td) -> add_type_declaration bv td) dcls; bv
  | Pstr_exception(id, args) ->
      List.iter (add_type bv) args; bv
  | Pstr_module(id, modl) ->
      add_module bv modl; StringSet.add id bv
  | Pstr_modtype(id, mty) ->
      add_modtype bv mty; bv
  | Pstr_open l ->
      addmodule bv l; bv
  | Pstr_class cdl ->
      List.iter (add_class_declaration bv) cdl; bv
  | Pstr_class_type cdtl ->
      List.iter (add_class_type_declaration bv) cdtl; bv

and add_class_expr bv ce =
  match ce.pcl_desc with
    Pcl_constr(l, tyl) ->
      add bv l; List.iter (add_type bv) tyl
  | Pcl_structure(pat, fieldl) ->
      add_pattern bv pat; List.iter (add_class_field bv) fieldl
  | Pcl_fun(pat, ce) ->
      add_pattern bv pat; add_class_expr bv ce
  | Pcl_apply(ce, exprl) ->
      add_class_expr bv ce; List.iter (add_expr bv) exprl
  | Pcl_let(_, pel, ce) ->
      add_pat_expr_list bv pel; add_class_expr bv ce
  | Pcl_constraint(ce, ct) ->
      add_class_expr bv ce; add_class_type bv ct

and add_class_field bv = function
    Pcf_inher(ce, _) -> add_class_expr bv ce
  | Pcf_val(_, _, e, _) -> add_expr bv e
  | Pcf_virt(_, _, ty, _) -> add_type bv ty
  | Pcf_meth(_, _, e, _) -> add_expr bv e
  | Pcf_cstr(ty1, ty2, _) -> add_type bv ty1; add_type bv ty2
  | Pcf_let(_, pel, _) -> add_pat_expr_list bv pel
  | Pcf_init e -> add_expr bv e

and add_class_declaration bv decl =
  add_class_expr bv decl.pci_expr

(* Print the dependencies *)

let load_path = ref [""]

let find_dependency modname (byt_deps, opt_deps) =
  let name = String.uncapitalize modname in
  try
    let filename = Misc.find_in_path !load_path (name ^ ".mli") in
    let basename = Filename.chop_suffix filename ".mli" in
    ((basename ^ ".cmi") :: byt_deps,
     (if Sys.file_exists (basename ^ ".ml")
      then basename ^ ".cmx"
      else basename ^ ".cmi") :: opt_deps)
  with Not_found ->
  try
    let filename = Misc.find_in_path !load_path (name ^ ".ml") in
    let basename = Filename.chop_suffix filename ".ml" in
    ((basename ^ ".cmo") :: byt_deps,
     (basename ^ ".cmx") :: opt_deps)
  with Not_found ->
    (byt_deps, opt_deps)

let (depends_on, escaped_eol) =
  match Sys.os_type with
  | "Unix" | "Win32" -> (": ", "\\\n    ")
  | "MacOS" -> ("\196 ", "\182\n    ")
  | _ -> assert false

let print_dependencies target_file deps =
  match deps with
    [] -> ()
  | _ ->
    print_string target_file; print_string depends_on;
    let rec print_items pos = function
      [] -> print_string "\n"
    | dep :: rem ->
        if pos + String.length dep <= 77 then begin
          print_string dep; print_string " ";
          print_items (pos + String.length dep + 1) rem
        end else begin
          print_string escaped_eol; print_string dep; print_string " ";
          print_items (String.length dep + 5) rem
        end in
    print_items (String.length target_file + 2) deps

let file_dependencies source_file =
  Location.input_name := source_file;
  try
    free_structure_names := StringSet.empty;
    let ic = open_in source_file in
    let lb = Lexing.from_channel ic in
    if Filename.check_suffix source_file ".ml" then begin
      add_structure StringSet.empty (Parse.implementation lb);
      let basename = Filename.chop_suffix source_file ".ml" in
      let init_deps =
        if Sys.file_exists (basename ^ ".mli")
        then let cmi_name = basename ^ ".cmi" in ([cmi_name], [cmi_name])
        else ([], []) in
      let (byt_deps, opt_deps) =
        StringSet.fold find_dependency !free_structure_names init_deps in
      print_dependencies (basename ^ ".cmo") byt_deps;
      print_dependencies (basename ^ ".cmx") opt_deps
    end else
    if Filename.check_suffix source_file ".mli" then begin
      add_signature StringSet.empty (Parse.interface lb);
      let basename = Filename.chop_suffix source_file ".mli" in
      let (byt_deps, opt_deps) =
        StringSet.fold find_dependency !free_structure_names ([], []) in
      print_dependencies (basename ^ ".cmi") byt_deps
    end else
      ();
    close_in ic
  with Sys_error msg ->
    ()

(* Entry point *)

open Format

let usage = "Usage: ocamldep [-I <dir>] <files>"

let _ =
  try
    Arg.parse [
       "-I", Arg.String(fun dir -> load_path := !load_path @ [dir]),
             "<dir>  Add <dir> to the list of include directories"
      ] file_dependencies usage;
    exit 0
  with x ->
    set_formatter_out_channel stderr;
    open_box 0;
    begin match x with
      Lexer.Error(err, start, stop) ->
        Location.print {loc_start = start; loc_end = stop};
        Lexer.report_error err
    | Syntaxerr.Error err ->
        Syntaxerr.report_error err
    | Sys_error msg ->
        print_string "I/O error: "; print_string msg
    | _ ->
        close_box(); raise x
    end;
    close_box(); print_newline(); exit 2


