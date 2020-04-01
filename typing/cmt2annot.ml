(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Generate an .annot file from a .cmt file. *)

open Asttypes
open Typedtree
open Tast_iterator

let variables_iterator scope =
  let super = default_iterator in
  let pat sub (type k) (p : k general_pattern) =
    begin match p.pat_desc with
    | Tpat_var (id, _) | Tpat_alias (_, id, _) ->
        Stypes.record (Stypes.An_ident (p.pat_loc,
                                        Ident.name id,
                                        Annot.Idef scope))
    | _ -> ()
    end;
    super.pat sub p
  in
  {super with pat}

let bind_variables scope =
  let iter = variables_iterator scope in
  fun p -> iter.pat iter p

let bind_bindings scope bindings =
  let o = bind_variables scope in
  List.iter (fun x -> o x.vb_pat) bindings

let bind_cases l =
  List.iter
    (fun {c_lhs; c_guard; c_rhs} ->
      let loc =
        let open Location in
        match c_guard with
        | None -> c_rhs.exp_loc
        | Some g -> {c_rhs.exp_loc with loc_start=g.exp_loc.loc_start}
      in
      bind_variables loc c_lhs
    )
    l

let record_module_binding scope mb =
  Stypes.record (Stypes.An_ident
                   (mb.mb_name.loc,
                    Option.value mb.mb_name.txt ~default:"_",
                    Annot.Idef scope))

let rec iterator ~scope rebuild_env =
  let super = default_iterator in
  let class_expr sub node =
    Stypes.record (Stypes.Ti_class node);
    super.class_expr sub node

  and module_expr _sub node =
    Stypes.record (Stypes.Ti_mod node);
    super.module_expr (iterator ~scope:node.mod_loc rebuild_env) node

  and expr sub exp =
    begin match exp.exp_desc with
    | Texp_ident (path, _, _) ->
        let full_name = Path.name ~paren:Oprint.parenthesized_ident path in
        let env =
          if rebuild_env then
            Env.env_of_only_summary Envaux.env_from_summary exp.exp_env
          else
            exp.exp_env
        in
        let annot =
          try
            let desc = Env.find_value path env in
            let dloc = desc.Types.val_loc in
            if dloc.Location.loc_ghost then Annot.Iref_external
            else Annot.Iref_internal dloc
          with Not_found ->
            Annot.Iref_external
        in
        Stypes.record
          (Stypes.An_ident (exp.exp_loc, full_name , annot))
    | Texp_let (Recursive, bindings, _) ->
        bind_bindings exp.exp_loc bindings
    | Texp_let (Nonrecursive, bindings, body) ->
        bind_bindings body.exp_loc bindings
    | Texp_match (_, f1, _) ->
        bind_cases f1
    | Texp_function { cases = f; }
    | Texp_try (_, f) ->
        bind_cases f
    | Texp_letmodule (_, modname, _, _, body ) ->
        Stypes.record (Stypes.An_ident
                         (modname.loc,Option.value ~default:"_" modname.txt,
                          Annot.Idef body.exp_loc))
    | _ -> ()
    end;
    Stypes.record (Stypes.Ti_expr exp);
    super.expr sub exp

  and pat sub (type k) (p : k general_pattern) =
    Stypes.record (Stypes.Ti_pat (classify_pattern p, p));
    super.pat sub p
  in

  let structure_item_rem sub str rem =
    let open Location in
    let loc = str.str_loc in
    begin match str.str_desc with
    | Tstr_value (rec_flag, bindings) ->
        let doit loc_start = bind_bindings {scope with loc_start} bindings in
        begin match rec_flag, rem with
        | Recursive, _ -> doit loc.loc_start
        | Nonrecursive, [] -> doit loc.loc_end
        | Nonrecursive,  {str_loc = loc2} :: _ -> doit loc2.loc_start
        end
    | Tstr_module mb ->
        record_module_binding
          { scope with Location.loc_start = loc.loc_end } mb
    | Tstr_recmodule mbs ->
        List.iter (record_module_binding
                   { scope with Location.loc_start = loc.loc_start }) mbs
    | _ ->
        ()
    end;
    Stypes.record_phrase loc;
    super.structure_item sub str
  in
  let structure_item sub s =
    (* This will be used for Partial_structure_item.
       We don't have here the location of the "next" item,
       this will give a slightly different scope for the non-recursive
       binding case. *)
    structure_item_rem sub s []
  in
  let structure sub l =
    let rec loop = function
      | str :: rem -> structure_item_rem sub str rem; loop rem
      | [] -> ()
    in
    loop l.str_items
  in
  {super with class_expr; module_expr; expr; pat; structure_item; structure}

let binary_part iter x =
  let open Cmt_format in
  match x with
  | Partial_structure x -> iter.structure iter x
  | Partial_structure_item x -> iter.structure_item iter x
  | Partial_expression x -> iter.expr iter x
  | Partial_pattern (_, x) -> iter.pat iter x
  | Partial_class_expr x -> iter.class_expr iter x
  | Partial_signature x -> iter.signature iter x
  | Partial_signature_item x -> iter.signature_item iter x
  | Partial_module_type x -> iter.module_type iter x

let gen_annot target_filename ~sourcefile ~use_summaries annots =
  let open Cmt_format in
  let scope =
    match sourcefile with
    | None -> Location.none
    | Some s -> Location.in_file s
  in
  let iter = iterator ~scope use_summaries in
  match annots with
  | Implementation typedtree ->
      iter.structure iter typedtree;
      Stypes.dump target_filename
  | Partial_implementation parts ->
      Array.iter (binary_part iter) parts;
      Stypes.dump target_filename
  | Interface _ | Packed _ | Partial_interface _ ->
      ()
