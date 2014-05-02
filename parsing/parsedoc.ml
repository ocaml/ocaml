(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                              Leo White                              *)
(*                                                                     *)
(*  Copyright 2013 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Analysis of inline documentation. *)
open Parsetree
open Documentation
open Lexing
open Location
open Comments

(* Function for reporting errors *)
let report_error =
  ref (fun loc err ->
         let message = Docerr.error_message err in
           Location.prerr_warning loc (Warnings.Documentation message))


(* Utilities for positions and locations *)

let pos_greater p1 p2 = p1.pos_cnum > p2.pos_cnum

let pos_max p1 p2 = if pos_greater p1 p2 then p1 else p2

let loc_greater l1 l2 = pos_greater l1.loc_start l2.loc_end

let is_ghost l = l.Location.loc_ghost

(* Adjust locations from the lexer/parser based on a starting position *)

let adjust_pos base p =
  { base with pos_cnum = base.pos_cnum + p.pos_cnum;
              pos_lnum = base.pos_lnum + p.pos_lnum - 1;
              pos_bol = if p.pos_bol = 0 then base.pos_bol
                        else base.pos_cnum + p.pos_bol }

let adjust_loc base l =
  { l with loc_start = adjust_pos base l.loc_start;
           loc_end = adjust_pos base l.loc_end; }

let adjust_comment base = function
    Special(s, loc) -> Special(s, adjust_loc base loc)
  | Simple -> Simple
  | Blank_line -> Blank_line
  | Stop loc -> Stop (adjust_loc base loc)

(* Functions for obtaining documentation from comments *)

(* Lex the comments within (optional) bound, before
   (optional) finish and after (optional) start *)
let lex_comments ?bound ?start ?finish file =
  let (fname, contents) = file in
  let start_pos =
    match bound, start with
    | Some bound, Some start -> pos_max bound.loc_start start.loc_end
    | None, Some start -> start.loc_end
    | Some bound, None -> bound.loc_start
    | None, None ->
        { pos_fname = fname;
          pos_lnum = 1;
          pos_bol = 0;
          pos_cnum = 0; }
  in
  let finish_cnum =
    match bound, finish with
    | Some bound, Some finish ->
        min bound.loc_end.pos_cnum finish.loc_start.pos_cnum
    | None, Some finish -> finish.loc_start.pos_cnum
    | Some bound, None -> bound.loc_end.pos_cnum
    | None, None -> String.length contents
  in
  let s =
    try
      String.sub contents start_pos.pos_cnum
                (finish_cnum - start_pos.pos_cnum)
    with Invalid_argument _ -> ""
  in
  let comments =
    try
      Comments.lex s
    with Docerr.Error(loc, err) ->
      !report_error (adjust_loc start_pos loc) err;
      []
  in
    List.map (adjust_comment start_pos) comments

(* Parse documentation from a string *)
let parse_info str =
  let lexbuf = Lexing.from_string str in
  Docparser.main Doclexer.main lexbuf

(* Create documentation attribute *)
let doc_attr loc comment =
  (Location.mknoloc "doc", PDoc(comment, loc))

(* Create comment attribute *)
let comment_attr loc comment =
  (Location.mknoloc "comment", PDoc(comment, loc))

(* To extract comments immediately following a signature item *)
let just_after_comment coms =
  match coms with
  | [] -> [], []
  | Special(str, loc) :: rest ->
      let start = loc.loc_start in
      let attrs =
        try
          [doc_attr loc (parse_info str)]
        with Docerr.Error(loc, err) ->
          !report_error (adjust_loc start loc) err;
          []
      in
        attrs, rest
  | _ :: rest -> [], coms

(* To extract comments immediately preceding a structure or signature item *)
let just_before_comment coms =
  let rec loop coms =
    match coms with
    | [] -> [], []
    | Special(str, loc) :: rest ->
        let start = loc.loc_start in
        let attrs =
          try
            [doc_attr loc (parse_info str)]
          with Docerr.Error(loc, err) ->
            !report_error (adjust_loc start loc) err;
            []
        in
          attrs, rest
    | Simple :: rest -> loop rest
    | Blank_line :: rest -> [], rest
    | Stop _ :: rest -> [], coms
  in
  let com, rest = loop (List.rev coms) in
    com, List.rev rest

(* To extract the first comment in a file *)
let rec first_comment = function
  | [] -> [], []
  | Special(str, loc) :: rest ->
      let start = loc.loc_start in
      let attrs =
        try
          [comment_attr loc (parse_info str)]
        with Docerr.Error(loc, err) ->
          !report_error (adjust_loc start loc) err;
          []
      in
        attrs, rest
  | Simple :: rest -> first_comment rest
  | Blank_line :: rest -> first_comment rest
  | Stop loc :: rest -> [comment_attr loc Cstop], rest

(* Filter special comments *)
let special_comments coms =
  let rec loop acc = function
    | [] -> List.rev acc
    | Special(str, loc) :: rest ->
      let start = loc.loc_start in
      let acc =
        try
          (comment_attr loc (parse_info str)) :: acc
        with Docerr.Error(loc, err) ->
          !report_error (adjust_loc start loc) err;
          acc
      in
        loop acc rest
    | Simple :: rest -> loop acc rest
    | Blank_line :: rest -> loop acc rest
    | Stop loc :: rest -> loop ((comment_attr loc Cstop) :: acc) rest
  in
    loop [] coms

(* Create items for comments not attached to another item *)
let comment_items attr_item coms =
  match attr_item with
    None -> []
  | Some attr_item ->
      List.map attr_item (special_comments coms)

(* Parse the contents of [@@doc "..."] attributes *)
let parse_attribute = function
  | ({txt = "doc"},
     PStr [{pstr_desc = Pstr_eval(
                          {pexp_desc =
                             Pexp_constant(Asttypes.Const_string(str, _));
                           pexp_loc = loc},
                        [])}]) as attr -> begin
      let start = loc.loc_start in
      try
          doc_attr loc (parse_info str)
        with Docerr.Error(loc, err) ->
          !report_error (adjust_loc start loc) err;
          attr
      end
  | attr -> attr

let parse_attrs attrs = List.map parse_attribute attrs

(* Utilities for parsing lists of items *)

(* Get next item that is after the given item *)
let next_item item_loc item rest =
  let loc = item_loc item in
  let rec loop ignored l =
    match l with
    | [] -> None, ignored
    | item :: rest ->
        let loc' = item_loc item in
          if not (is_ghost loc') && loc_greater loc' loc then
            Some(item, l), ignored
          else
            loop (item :: ignored) rest
  in
    loop [] rest

(* Get the first item in a list *)
let first_item item_loc items =
  let rec loop ignored = function
      [] -> None, ignored
    | item :: rest ->
        if not (is_ghost (item_loc item)) then
          Some(item, rest), ignored
        else
          loop (item :: ignored) rest
  in
    loop [] items

(* Get the last item in a list *)
let last_item item_loc items =
  let rec loop acc = function
    | [] -> Some acc
    | item :: rest ->
        let loc = item_loc item in
          if not (is_ghost loc) && loc_greater loc (item_loc acc) then
            loop item rest
          else
            loop acc rest
  in
    match first_item item_loc items with
    | None, _ -> None
    | Some(first, rest), _ -> loop first rest

(* Parse a list of items within a given location *)
let parse_closed_list ~file ~item_loc ~parse_item ?attr_item ~bound items =
  let rec loop before acc = function
    | [] ->
        let extra = comment_items attr_item before in
          List.rev_append acc extra
    | item :: rest ->
        let next, ignored = next_item item_loc item rest in
        let after, rest =
          match next with
          | None ->
              let start = item_loc item in
              let after = lex_comments ~bound ~start file in
                after, rest
          | Some(next, rest) ->
              let start = item_loc item in
              let finish = item_loc next in
              let after = lex_comments ~bound ~start ~finish file in
                after, rest
        in
        let item, before, after = parse_item file before after item in
        let extra = comment_items attr_item before in
        let acc = ignored @ (item :: (List.rev_append extra acc)) in
          loop after acc rest
  in
    match first_item item_loc items with
    | None, ignored ->
        let extra = comment_items attr_item (lex_comments ~bound file) in
          List.rev_append ignored extra
    | Some(item, rest), ignored ->
        let finish = item_loc item in
        let before = lex_comments ~bound ~finish file in
          loop before ignored (item :: rest)

(* Parse a list of items without bounds *)
let parse_open_list ~file ~item_loc ~parse_item ?attr_item ~before ~after items =
  let rec loop before acc = function
    | [] -> assert false
    | item :: rest ->
        let next, ignored = next_item item_loc item rest in
          match next with
          | None ->
              let item, before, after =
                parse_item false file before after item
              in
              let extra = comment_items attr_item before in
              let items =
                List.rev_append acc (extra @ (item :: (List.rev ignored)))
              in
                items, after
          | Some(next, rest) ->
              let start = item_loc item in
              let finish = item_loc next in
              let after = lex_comments ~start ~finish file in
              let item, before, after =
                parse_item false file before after item
              in
              let extra = comment_items attr_item before in
              let acc = ignored @ (item :: (List.rev_append extra acc)) in
                loop after acc rest
  in
    match first_item item_loc items with
    | None, _ -> items, before, after
    | Some(item, rest), pre_ignored ->
        let next, ignored = next_item item_loc item rest in
          match next with
          | None ->
              let item, before, after =
                parse_item true file before after item
              in
              let items =
                List.rev_append pre_ignored (item :: (List.rev ignored))
              in
                items, before, after
          | Some(next, rest) ->
              let start = item_loc item in
              let finish = item_loc next in
              let after = lex_comments ~start ~finish file in
              let item, before, after =
                parse_item true file before after item
              in
              let acc = ignored @ (item :: pre_ignored) in
              let items, after = loop after acc rest in
                items, before, after

(* Value bindings *)

let value_binding first file before after vb =
  let docs, before = just_before_comment before in
  let comments, before =
    if first then [], before
    else special_comments before, []
  in
  let attrs = docs @ comments @ parse_attrs vb.pvb_attributes in
  let vb = { vb with pvb_attributes = attrs }
  in
    vb, before, after

(* Type declarations *)

let constructor_declaration _first file before after cd =
  let docs, after = just_after_comment after in
  let attrs = docs @ parse_attrs cd.pcd_attributes in
  let cd = { cd with pcd_attributes = attrs } in
    cd, before, after

let label_declaration file before after ld =
  let docs, after = just_after_comment after in
  let attrs = docs @ parse_attrs ld.pld_attributes in
  let ld = { ld with pld_attributes = attrs } in
    ld, before, after

let type_kind ~file ~bound ~after = function
    Ptype_abstract -> Ptype_abstract, after
  | Ptype_variant cstrs ->
      let item_loc cd = cd.pcd_loc in
      let cstrs, _, after =
        parse_open_list ~file ~item_loc
          ~parse_item:constructor_declaration
          ~before:[] ~after cstrs
      in
        Ptype_variant cstrs, after
  | Ptype_record fields ->
      let item_loc ld = ld.pld_loc in
      let fields =
        parse_closed_list ~file ~item_loc
          ~parse_item:label_declaration
          ~bound fields
      in
        Ptype_record fields, after

let type_declaration_sig first file before after typ =
  let docs, before = just_before_comment before in
  let comments, before =
    if first then [], before
    else special_comments before, []
  in
  let kind, after =
    type_kind ~file ~bound:typ.ptype_loc ~after typ.ptype_kind
  in
  let after_docs, after = just_after_comment after in
  let attrs =
    docs @ after_docs @ comments @ parse_attrs typ.ptype_attributes
  in
  let typ = { typ with ptype_kind = kind;
                       ptype_attributes = attrs }
  in
    typ, before, after

let type_declaration_str first file before after typ =
  let docs, before = just_before_comment before in
  let comments, before =
    if first then [], before
    else special_comments before, []
  in
  let kind, after =
    type_kind ~file ~bound:typ.ptype_loc ~after typ.ptype_kind
  in
  let attrs = docs @ comments @ parse_attrs typ.ptype_attributes in
  let typ = { typ with ptype_kind = kind;
                       ptype_attributes = attrs }
  in
    typ, before, after

(* Class types *)

let rec class_type_field file before after field =
  match field.pctf_desc with
  | Pctf_inherit cty ->
      let docs, before = just_before_comment before in
      let cty = class_type file cty in
      let after_docs, after = just_after_comment after in
      let attrs = docs @ after_docs @ parse_attrs field.pctf_attributes in
      let field = { field with pctf_desc = Pctf_inherit cty;
                               pctf_attributes = attrs }
      in
        field, before, after
  | Pctf_val _ | Pctf_method _  ->
      let docs, before = just_before_comment before in
      let after_docs, after = just_after_comment after in
      let attrs = docs @ after_docs @ parse_attrs field.pctf_attributes in
      let field = { field with pctf_attributes = attrs } in
        field, before, after
  | Pctf_attribute attr ->
      let attr = parse_attribute attr in
      let field = { field with pctf_desc = Pctf_attribute attr } in
        field, before, after
  | Pctf_constraint _ | Pctf_extension _ ->
      field, before, after

and class_type file cty =
  match cty.pcty_desc with
  | Pcty_arrow(lbl, arg, body) ->
      let body = class_type file body in
        { cty with pcty_desc = Pcty_arrow(lbl, arg, body) }
  | Pcty_signature csig ->
      let item_loc ctf = ctf.pctf_loc in
      let attr_item attr =
        { pctf_desc = Pctf_attribute attr;
          pctf_loc = Location.none;
          pctf_attributes = [] }
      in
      let fields =
        parse_closed_list ~file ~item_loc
          ~parse_item:class_type_field
          ~attr_item ~bound:cty.pcty_loc csig.pcsig_fields
      in
      let csig = { csig with pcsig_fields = fields } in
        { cty with pcty_desc = Pcty_signature csig }
  | Pcty_constr _ | Pcty_extension _ -> cty

let class_type_declaration_sig first file before after ci =
  let docs, before = just_before_comment before in
  let comments, before =
    if first then [], before
    else special_comments before, []
  in
  let expr = class_type file ci.pci_expr in
  let after_docs, after =just_after_comment after in
  let attrs = docs @ after_docs @ comments @ parse_attrs ci.pci_attributes in
  let ci = { ci with pci_expr = expr;
                     pci_attributes = attrs }
  in
    ci, before, after

let class_type_declaration_str first file before after ci =
  let docs, before = just_before_comment before in
  let comments, before =
    if first then [], before
    else special_comments before, []
  in
  let expr = class_type file ci.pci_expr in
  let attrs = docs @ comments @ parse_attrs ci.pci_attributes in
  let ci = { ci with pci_expr = expr;
                     pci_attributes = attrs }
  in
    ci, before, after

let class_description first file before after ci =
  let docs, before = just_before_comment before in
  let comments, before =
    if first then [], before
    else special_comments before, []
  in
  let expr = class_type file ci.pci_expr in
  let after_docs, after = just_after_comment after in
  let attrs = docs @ after_docs @ comments @ parse_attrs ci.pci_attributes in
  let ci = { ci with pci_expr = expr;
                     pci_attributes = attrs }
  in
    ci, before, after

(* Classes *)

let rec class_field file before after field =
  match field.pcf_desc with
  | Pcf_inherit(ov, cl, so) ->
      let docs, before = just_before_comment before in
      let cl = class_expr file cl in
      let attrs = docs @ parse_attrs field.pcf_attributes in
      let field = { field with pcf_desc = Pcf_inherit(ov, cl, so);
                               pcf_attributes = attrs }
      in
        field, before, after
  | Pcf_val _ | Pcf_method _ ->
      let docs, before = just_before_comment before in
      let attrs = docs @ parse_attrs field.pcf_attributes in
      let field = { field with pcf_attributes = attrs }
      in
        field, before, after
  | Pcf_attribute attr ->
      let attr = parse_attribute attr in
      let field = { field with pcf_desc = Pcf_attribute attr } in
        field, before, after
  | Pcf_constraint _ | Pcf_initializer _ | Pcf_extension _ ->
      field, before, after

and class_expr file cl =
  match cl.pcl_desc with
  | Pcl_fun(lbl, eo, pat, body) ->
      let body = class_expr file body in
        { cl with pcl_desc = Pcl_fun(lbl, eo, pat, body) }
  | Pcl_apply(body, l) ->
      let body = class_expr file body in
        { cl with pcl_desc = Pcl_apply(body, l) }
  | Pcl_let(rf, vb, body) ->
      let body = class_expr file body in
        { cl with pcl_desc = Pcl_let(rf, vb, body) }
  | Pcl_constraint(body, cty) ->
      let body = class_expr file body in
      let cty = class_type file cty in
        { cl with pcl_desc = Pcl_constraint(body, cty) }
  | Pcl_structure cstr ->
      let item_loc cf = cf.pcf_loc in
      let attr_item attr =
        { pcf_desc = Pcf_attribute attr;
          pcf_loc = Location.none;
          pcf_attributes = [] }
      in
      let fields =
        parse_closed_list ~file ~item_loc
          ~parse_item:class_field
          ~attr_item ~bound:cl.pcl_loc cstr.pcstr_fields
      in
      let cstr = { cstr with pcstr_fields = fields } in
        { cl with pcl_desc = Pcl_structure cstr }
  | Pcl_constr _ | Pcl_extension _ -> cl


let class_declaration first file before after ci =
  let docs, before = just_before_comment before in
  let comments, before =
    if first then [], before
    else special_comments before, []
  in
  let expr = class_expr file ci.pci_expr in
  let attrs = docs @ comments @ parse_attrs ci.pci_attributes in
  let ci = { ci with pci_expr = expr;
                     pci_attributes = attrs }
  in
    ci, before, after

(* Module types *)

let rec signature_item file before after item =
  match item.psig_desc with
  | Psig_value vd ->
      let docs, before = just_before_comment before in
      let after_docs, after = just_after_comment after in
      let attrs = docs @ after_docs @ parse_attrs vd.pval_attributes in
      let vd = { vd with pval_attributes = attrs }
      in
        { item with psig_desc = Psig_value vd }, before, after
  | Psig_type typs ->
      let item_loc typ = typ.ptype_loc in
      let typs, before, after =
        parse_open_list ~file ~item_loc
          ~parse_item:type_declaration_sig
          ~before ~after typs
      in
        { item with psig_desc = Psig_type typs }, before, after
  | Psig_exception cd ->
      let docs, before = just_before_comment before in
      let after_docs, after = just_after_comment after in
      let attrs = docs @ after_docs @ parse_attrs cd.pcd_attributes in
      let cd = { cd with pcd_attributes = attrs }
      in
        { item with psig_desc = Psig_exception cd }, before, after
  | Psig_module md ->
      let md, before, after =
        module_declaration true file before after md
      in
        { item with psig_desc = Psig_module md }, before, after
  | Psig_recmodule mds ->
      let item_loc md = md.pmd_loc in
      let mds, before, after =
        parse_open_list ~file ~item_loc
          ~parse_item:module_declaration
          ~before ~after mds
      in
        { item with psig_desc = Psig_recmodule mds }, before, after
  | Psig_modtype mtd ->
      let docs, before = just_before_comment before in
      let mty = Misc.may_map (module_type file) mtd.pmtd_type in
      let after_docs, after = just_after_comment after in
      let attrs = docs @ after_docs @ parse_attrs mtd.pmtd_attributes in
      let mtd = { mtd with pmtd_type = mty;
                           pmtd_attributes = attrs }
      in
        { item with psig_desc = Psig_modtype mtd }, before, after
  | Psig_include incl ->
      let docs, before = just_before_comment before in
      let mty = module_type file incl.pincl_mod in
      let after_docs, after = just_after_comment after in
      let attrs = docs @ after_docs @ parse_attrs incl.pincl_attributes in
      let incl = { incl with pincl_mod = mty;
                             pincl_attributes = attrs }
      in
        { item with psig_desc = Psig_include incl }, before, after
 | Psig_class clds ->
      let item_loc cld = cld.pci_loc in
      let clds, before, after =
        parse_open_list ~file ~item_loc
          ~parse_item:class_description
          ~before ~after clds
      in
        { item with psig_desc = Psig_class clds }, before, after
  | Psig_class_type cltyds ->
      let item_loc cltyd = cltyd.pci_loc in
      let cltyds, before, after =
        parse_open_list ~file ~item_loc
          ~parse_item:class_type_declaration_sig
          ~before ~after cltyds
      in
        { item with psig_desc = Psig_class cltyds }, before, after
  | Psig_attribute attr ->
      let attr = parse_attribute attr in
        { item with psig_desc = Psig_attribute attr }, before, after
  | Psig_open _ | Psig_extension _ ->
      item, before, after

and module_type file mty =
  match mty.pmty_desc with
  | Pmty_functor(s, arg, body) ->
      let arg = Misc.may_map (module_type file) arg in
      let body = module_type file body in
        { mty with pmty_desc = Pmty_functor(s, arg, body) }
  | Pmty_with(body, l) ->
      let body = module_type file body in
        { mty with pmty_desc = Pmty_with(body, l) }
  | Pmty_signature items ->
      let item_loc item = item.psig_loc in
      let attr_item attr =
        { psig_desc = Psig_attribute attr;
          psig_loc = Location.none }
      in
      let items =
        parse_closed_list ~file ~item_loc
          ~parse_item:signature_item
          ~attr_item ~bound:mty.pmty_loc items
      in
        { mty with pmty_desc = Pmty_signature items }
  | Pmty_ident _ | Pmty_alias _ | Pmty_typeof _ | Pmty_extension _ -> mty


and module_declaration first file before after md =
  let docs, before = just_before_comment before in
  let comments, before =
    if first then [], before
    else special_comments before, []
  in
  let mty = module_type file md.pmd_type in
  let after_docs, after = just_after_comment after in
  let attrs = docs @ after_docs @ comments @ parse_attrs md.pmd_attributes in
  let md = { md with pmd_type = mty;
                     pmd_attributes = attrs }
  in
    md, before, after

(* Modules *)

and structure_item file before after item =
  match item.pstr_desc with
  | Pstr_value(rf, vbs) ->
      let item_loc vb = vb.pvb_loc in
      let vbs, before, after =
        parse_open_list ~file ~item_loc
          ~parse_item:value_binding
          ~before ~after vbs
      in
        { item with pstr_desc = Pstr_value(rf, vbs) }, before, after
  | Pstr_primitive vd ->
      let docs, before = just_before_comment before in
      let attrs = docs @ parse_attrs vd.pval_attributes in
      let vd = { vd with pval_attributes = attrs }
      in
        { item with pstr_desc = Pstr_primitive vd }, before, after
  | Pstr_type typs ->
      let item_loc typ = typ.ptype_loc in
      let typs, before, after =
        parse_open_list ~file ~item_loc
          ~parse_item:type_declaration_str
          ~before ~after typs
      in
        { item with pstr_desc = Pstr_type typs }, before, after
  | Pstr_exception cd ->
      let docs, before = just_before_comment before in
      let attrs = docs @ parse_attrs cd.pcd_attributes in
      let cd = { cd with pcd_attributes = attrs }
      in
        { item with pstr_desc = Pstr_exception cd }, before, after
  | Pstr_exn_rebind exrb ->
      let docs, before = just_before_comment before in
      let attrs = docs @ parse_attrs exrb.pexrb_attributes in
      let exrb = { exrb with pexrb_attributes = attrs }
      in
        { item with pstr_desc = Pstr_exn_rebind exrb }, before, after
  | Pstr_module mb ->
      let mb, before, after =
        module_binding true file before after mb
      in
        { item with pstr_desc = Pstr_module mb }, before, after
  | Pstr_recmodule mbs ->
      let item_loc mb = mb.pmb_loc in
      let mbs, before, after =
        parse_open_list ~file ~item_loc
          ~parse_item:module_binding
          ~before ~after mbs
      in
        { item with pstr_desc = Pstr_recmodule mbs }, before, after
  | Pstr_modtype mtd ->
      let docs, before = just_before_comment before in
      let mty = Misc.may_map (module_type file) mtd.pmtd_type in
      let attrs = docs @ parse_attrs mtd.pmtd_attributes in
      let mtd = { mtd with pmtd_type = mty;
                           pmtd_attributes = attrs }
      in
        { item with pstr_desc = Pstr_modtype mtd }, before, after
  | Pstr_include incl ->
      let docs, before = just_before_comment before in
      let mexpr = module_expr file incl.pincl_mod in
      let attrs = docs @ parse_attrs incl.pincl_attributes in
      let incl = { incl with pincl_mod = mexpr;
                             pincl_attributes = attrs }
      in
        { item with pstr_desc = Pstr_include incl }, before, after
  | Pstr_class clds ->
      let item_loc cld = cld.pci_loc in
      let clds, before, after =
        parse_open_list ~file ~item_loc
          ~parse_item:class_declaration
          ~before ~after clds
      in
        { item with pstr_desc = Pstr_class clds }, before, after
  | Pstr_class_type cltyds ->
      let item_loc cltyd = cltyd.pci_loc in
      let cltyds, before, after =
        parse_open_list ~file ~item_loc
          ~parse_item:class_type_declaration_str
          ~before ~after cltyds
      in
        { item with pstr_desc = Pstr_class_type cltyds }, before, after
  | Pstr_attribute attr ->
      let attr = parse_attribute attr in
        { item with pstr_desc = Pstr_attribute attr }, before, after
  | Pstr_eval _ | Pstr_open _ | Pstr_extension _ ->
      item, before, after

and module_expr file mexpr =
  match mexpr.pmod_desc with
  | Pmod_functor(s, mty, body) ->
      let mty = Misc.may_map (module_type file) mty in
      let body = module_expr file body in
        { mexpr with pmod_desc = Pmod_functor(s, mty, body) }
  | Pmod_apply(func, arg) ->
      let func = module_expr file func in
      let arg = module_expr file arg in
        { mexpr with pmod_desc = Pmod_apply(func, arg) }
  | Pmod_constraint(body, mty) ->
      let body = module_expr file body in
      let mty = module_type file mty in
        { mexpr with pmod_desc = Pmod_constraint(body, mty) }
  | Pmod_structure items ->
      let item_loc item = item.pstr_loc in
      let attr_item attr =
        { pstr_desc = Pstr_attribute attr;
          pstr_loc = Location.none }
      in
      let items =
        parse_closed_list ~file ~item_loc
          ~parse_item:structure_item
          ~attr_item ~bound:mexpr.pmod_loc items
      in
        { mexpr with pmod_desc = Pmod_structure items }
  | Pmod_ident _ | Pmod_unpack _ | Pmod_extension _-> mexpr

and module_binding first file before after mb =
  let docs, before = just_before_comment before in
  let comments, before =
    if first then [], before
    else special_comments before, []
  in
  let expr = module_expr file mb.pmb_expr in
  let attrs = docs @ comments @ parse_attrs mb.pmb_attributes in
  let mb = { mb with pmb_expr = expr;
                     pmb_attributes = attrs }
  in
    mb, before, after

(* Interfaces and implementations *)

let read_file filename =
  let chanin = open_in_bin filename in
  let len = 1024 in
  let s = Bytes.create len in
  let buf = Buffer.create len in
  let rec iter () =
    try
      let n = input chanin s 0 len in
        if n = 0 then ()
        else begin
          Buffer.add_subbytes buf s 0 n;
          iter ()
        end
    with End_of_file -> ()
  in
  iter ();
  close_in chanin;
  Buffer.contents buf

let interface filename items =
  let file = (filename, read_file filename) in
  let item_loc item = item.psig_loc in
  let attr_item attr =
    { psig_desc = Psig_attribute attr;
      psig_loc = Location.none }
  in
  let parse_item _first file before after item =
    signature_item file before after item
  in
  let before, after =
    match first_item item_loc items with
    | None, _ -> lex_comments file, []
    | Some (first, _), _ ->
        let finish = item_loc first in
        let before = lex_comments ~finish file in
        let last = last_item item_loc items in
        let start = Misc.may_map item_loc last in
        let after = lex_comments ?start file in
          before, after
  in
  let comment, before = first_comment before in
  let first = List.map attr_item comment in
  let items, before, after =
    parse_open_list ~file ~item_loc ~parse_item
      ~attr_item ~before ~after items
  in
  let before_extra = comment_items (Some attr_item) before in
  let after_extra = comment_items (Some attr_item) after in
    match after_extra with
    | [] -> first @ before_extra @ items
    | _ -> first @ before_extra @
             (List.rev_append (List.rev items) after_extra)

let implementation filename items =
  let file = (filename, read_file filename) in
  let item_loc item = item.pstr_loc in
  let attr_item attr =
    { pstr_desc = Pstr_attribute attr;
      pstr_loc = Location.none }
  in
  let parse_item _first file before after item =
    structure_item file before after item
  in
  let before, after =
    match first_item item_loc items with
    | None, _ -> lex_comments file, []
    | Some (first, _), _ ->
        let finish = item_loc first in
        let before = lex_comments ~finish file in
        let last = last_item item_loc items in
        let start = Misc.may_map item_loc last in
        let after = lex_comments ?start file in
          before, after
  in
  let comment, before = first_comment before in
  let first = List.map attr_item comment in
  let items, before, after =
    parse_open_list ~file ~item_loc ~parse_item
      ~attr_item ~before ~after items
  in
  let before_extra = comment_items (Some attr_item) before in
  let after_extra = comment_items (Some attr_item) after in
    match after_extra with
    | [] -> first @ before_extra @ items
    | _ -> first @ before_extra @
             (List.rev_append (List.rev items) after_extra)
