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

(* Simplified version of Ctype.free_vars.  Also compute
   an ordered sequence of type variables (in the order in which
   they appear, syntactically). *)
let free_vars ty =
  let ret = ref TypeSet.empty in
  let l = ref [] in
  let rec loop ty =
    let ty = repr ty in
    if ty.level >= lowest_level then begin
      ty.level <- pivot_level - ty.level;
      match ty.desc with
      | Tvar _ ->
          if not (TypeSet.mem ty !ret) then begin
            ret := TypeSet.add ty !ret;
            l := ty :: !l
          end;
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
  !ret, List.rev !l

let newgenconstr path tyl = newgenty (Tconstr (path, tyl, ref Mnil))

let constructor_args name ty_path type_manifest arg_vars rep =
  function
  | Cstr_tuple l -> l, None
  | Cstr_record (_, lbls) ->
      let path = Path.Pdot(ty_path, name, Path.nopos) in
      let type_manifest =
        match type_manifest with
        | Some p -> Some (newgenconstr p arg_vars)
        | None -> None
      in
      let tdecl =
        {
          type_params = arg_vars;
          type_arity = List.length arg_vars;
          type_kind = Type_record (lbls, rep);
          type_private = Public;
          type_manifest;
          type_variance = List.map (fun _ -> Variance.full) arg_vars;
          type_newtype_level = None;
          type_loc = Location.none;
          type_attributes = [];
        }
      in
      [ newgenconstr path arg_vars ],
      Some tdecl

let constructor_descrs ty_path decl cstrs =
  let ty_res = newgenconstr ty_path decl.type_params in
  let num_consts = ref 0 and num_nonconsts = ref 0  and num_normal = ref 0 in
  List.iter
    (fun {cd_args; cd_res; _} ->
      if cd_args = Cstr_tuple [] then incr num_consts else incr num_nonconsts;
      if cd_res = None then incr num_normal)
    cstrs;
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
        let tyl =
          match cd_args with
          | Cstr_tuple l -> l
          | Cstr_record (_, l) -> List.map (fun l -> l.ld_type) l
        in
        (* Note: variables bound by Tpoly are Tvar, not Tunivar,
           and thus they are not considered as free, which is
           what we want. *)
        let arg_vars_set, arg_vars = free_vars (newgenty (Ttuple tyl)) in
        let existentials =
          match cd_res with
          | None -> []
          | Some type_ret ->
              let res_vars, _ = free_vars type_ret in
              TypeSet.elements (TypeSet.diff arg_vars_set res_vars)
        in
        let type_manifest =
          match decl.type_manifest with
          | Some {desc = Tconstr(p, _, _)} ->
              Some (Path.Pdot (p, Ident.name cd_id, Path.nopos))
          | _ -> None
        in
        let cstr_args, cstr_inlined =
          constructor_args (Ident.name cd_id) ty_path type_manifest
            arg_vars
            (Record_inlined idx_nonconst)
            cd_args
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
  describe_constructors 0 0 cstrs

let extension_descr ?rebind path_ext ext =
  let ty_res =
    match ext.ext_ret_type with
        Some type_ret -> type_ret
      | None -> newgenconstr ext.ext_type_path ext.ext_type_params
  in
  (* TODO #5528: merge code below with constructor_descrs *)
  let tyl =
    match ext.ext_args with
    | Cstr_tuple l -> l
    | Cstr_record (_, l) -> List.map (fun l -> l.ld_type) l
  in
  let arg_vars_set, arg_vars = free_vars (newgenty (Ttuple tyl)) in
  let existentials =
    match ext.ext_ret_type with
      | None -> []
      | Some type_ret ->
          let res_vars, _ = free_vars type_ret in
          TypeSet.elements (TypeSet.diff arg_vars_set res_vars)
  in
  let cstr_args, cstr_inlined =
    constructor_args (Path.last path_ext) path_ext rebind
      arg_vars
      (Record_extension path_ext)
      ext.ext_args
  in
    { cstr_name = Path.last path_ext;
      cstr_res = ty_res;
      cstr_existentials = existentials;
      cstr_args;
      cstr_arity = List.length cstr_args;
      cstr_tag = Cstr_extension(path_ext, cstr_args = []);
      cstr_consts = -1;
      cstr_nonconsts = -1;
      cstr_private = ext.ext_private;
      cstr_normal = -1;
      cstr_generalized = ext.ext_ret_type <> None;
      cstr_loc = ext.ext_loc;
      cstr_attributes = ext.ext_attributes;
      cstr_inlined;
    },
    []


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
  let all_labels = Array.make (List.length lbls) dummy_label in
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
