(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Compute constructor and label descriptions from type declarations,
   determining their representation. *)

open Asttypes
open Types
open Btype

(* Simplified version of Ctype.free_vars *)
let free_vars ty =
  let ret = ref TypeSet.empty in
  let rec loop ty =
    let ty = repr ty in
    if ty.level >= lowest_level then begin
      ty.level <- pivot_level - ty.level;
      match ty.desc with
      | Tvar _ ->
          ret := TypeSet.add ty !ret
      | Tvariant row ->
          let row = row_repr row in
          iter_row loop row;
          if not (static_row row) then loop row.row_more
      | _ ->
          iter_type_expr loop ty
    end
  in
  loop ty;
  unmark_type ty;
  !ret

let constructor_descrs ty_path decl cstrs =
  let ty_res = newgenty (Tconstr(ty_path, decl.type_params, ref Mnil)) in
  let num_consts = ref 0 and num_nonconsts = ref 0  and num_normal = ref 0 in
  List.iter
    (fun {cd_args; cd_res; _} ->
      if cd_args = Cstr_tuple [] then incr num_consts else incr num_nonconsts;
      if cd_res = None then incr num_normal)
    cstrs;
  let tdecls = ref [] in
  let rec describe_constructors idx_const idx_nonconst = function
      [] -> []
    | {cd_id; cd_args; cd_res; cd_loc; cd_attributes} :: rem ->
        let ty_res =
          match cd_res with
          | Some ty_res' -> ty_res'
          | None -> ty_res
        in
        let (tag, descr_rem) =
          match cd_args with
            Cstr_tuple [] -> (Cstr_constant idx_const,
                   describe_constructors (idx_const+1) idx_nonconst rem)
          | _  -> (Cstr_block idx_nonconst,
                   describe_constructors idx_const (idx_nonconst+1) rem) in
        let existentials =
          match cd_res with
          | None -> []
          | Some type_ret ->
              let res_vars = free_vars type_ret in
              let tyl =
                match cd_args with
                | Cstr_tuple l -> l
                | Cstr_record l -> List.map (fun l -> l.ld_type) l
              in
              (* TODO: handle Tpoly *)
              let arg_vars = free_vars (newgenty (Ttuple tyl)) in
              TypeSet.elements (TypeSet.diff arg_vars res_vars)
        in
        let cstr_args, cstr_inlined =
          match cd_args with
          | Cstr_tuple l -> l, false
          | Cstr_record lbls ->
              let name = Path.last ty_path ^ "." ^ Ident.name cd_id in
              let id = Ident.create name in
              let path =
                match ty_path with
                | Path.Pdot(m, _, _) -> Path.Pdot(m, name, Path.nopos)
                | Path.Pident _ -> Path.Pident id
                | Path.Papply _ -> assert false
              in
              let type_manifest =
                match decl.type_manifest with
                | Some {desc = Tconstr(Path.Pdot (m, name, _), args, _)} ->
                    let p = Path.Pdot (m, name ^ "." ^ Ident.name cd_id, Path.nopos) in
                    Some (newgenty (Tconstr (p, args, ref Mnil)))
                | Some {desc = Tconstr(Path.Pident _, args, _)} ->
                    None (* looses the identity! *)
                      (* could we retrieve it in the current environment? *)
                | _ -> None
              in
              let tdecl =
                {
                  type_params = decl.type_params; (* TODO: add existentials *)
                  type_arity = decl.type_arity;
                  type_kind = Type_record (lbls, Record_inlined idx_nonconst);
                  type_private = Public;
                  type_manifest;
                  type_variance = decl.type_variance;
                  type_newtype_level = None;
                  type_loc = Location.none;
                  type_attributes = [];
                }
              in
              tdecls := (id, path, tdecl) :: !tdecls;
              [ newgenty (Tconstr(path, decl.type_params, ref Mnil)) ],
              true
        in
        let cstr =
          { cstr_name = Ident.name cd_id;
            cstr_res = ty_res;
            cstr_existentials = existentials;
            cstr_args;
            cstr_arity = List.length cstr_args;
            cstr_tag = tag;
            cstr_consts = !num_consts;
            cstr_nonconsts = !num_nonconsts;
            cstr_normal = !num_normal;
            cstr_private = decl.type_private;
            cstr_generalized = cd_res <> None;
            cstr_loc = cd_loc;
            cstr_attributes = cd_attributes;
            cstr_inlined;
          } in
        (cd_id, cstr) :: descr_rem in
  let r = describe_constructors 0 0 cstrs in
  r, !tdecls

let exception_descr path_exc decl =
  { cstr_name = Path.last path_exc;
    cstr_res = Predef.type_exn;
    cstr_existentials = [];
    cstr_args = decl.exn_args;
    cstr_arity = List.length decl.exn_args;
    cstr_tag = Cstr_exception (path_exc, decl.exn_loc);
    cstr_consts = -1;
    cstr_nonconsts = -1;
    cstr_private = Public;
    cstr_normal = -1;
    cstr_generalized = false;
    cstr_loc = decl.exn_loc;
    cstr_attributes = decl.exn_attributes;
    cstr_inlined = false;
  }

let none = {desc = Ttuple []; level = -1; id = -1}
                                        (* Clearly ill-formed type *)
let dummy_label =
  { lbl_name = ""; lbl_res = none; lbl_arg = none; lbl_mut = Immutable;
    lbl_pos = (-1); lbl_all = [||]; lbl_repres = Record_regular;
    lbl_private = Public;
    lbl_loc = Location.none;
    lbl_attributes = [];
  }

let label_descrs ty_res lbls repres priv =
  let all_labels = Array.create (List.length lbls) dummy_label in
  let rec describe_labels num = function
      [] -> []
    | l :: rest ->
        let lbl =
          { lbl_name = Ident.name l.ld_id;
            lbl_res = ty_res;
            lbl_arg = l.ld_type;
            lbl_mut = l.ld_mutable;
            lbl_pos = num;
            lbl_all = all_labels;
            lbl_repres = repres;
            lbl_private = priv;
            lbl_loc = l.ld_loc;
            lbl_attributes = l.ld_attributes;
          } in
        all_labels.(num) <- lbl;
        (l.ld_id, lbl) :: describe_labels (num+1) rest in
  describe_labels 0 lbls

exception Constr_not_found

let rec find_constr tag num_const num_nonconst = function
    [] ->
      raise Constr_not_found
  | {cd_args = Cstr_tuple []; _} as c  :: rem ->
      if tag = Cstr_constant num_const
      then c
      else find_constr tag (num_const + 1) num_nonconst rem
  | c :: rem ->
      if tag = Cstr_block num_nonconst
      then c
      else find_constr tag num_const (num_nonconst + 1) rem

let find_constr_by_tag tag cstrlist =
  find_constr tag 0 0 cstrlist
