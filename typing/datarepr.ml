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

let constructor_descrs ty_res cstrs priv =
  let num_consts = ref 0 and num_nonconsts = ref 0  and num_normal = ref 0 in
  List.iter
    (fun (name, args, ret) ->
      if args = [] then incr num_consts else incr num_nonconsts;
      if ret = None then incr num_normal)
    cstrs;
  let rec describe_constructors idx_const idx_nonconst = function
      [] -> []
    | (id, ty_args, ty_res_opt) :: rem ->
        let ty_res =
          match ty_res_opt with
          | Some ty_res' -> ty_res'
          | None -> ty_res
        in
        let (tag, descr_rem) =
          match ty_args with
            [] -> (Cstr_constant idx_const,
                   describe_constructors (idx_const+1) idx_nonconst rem)
          | _  -> (Cstr_block idx_nonconst,
                   describe_constructors idx_const (idx_nonconst+1) rem) in
        let existentials =
          match ty_res_opt with
          | None -> []
          | Some type_ret ->
              let res_vars = free_vars type_ret in
              let arg_vars = free_vars (newgenty (Ttuple ty_args)) in
              TypeSet.elements (TypeSet.diff arg_vars res_vars)
        in
        let cstr =
          { cstr_name = Ident.name id;
            cstr_res = ty_res;
            cstr_existentials = existentials;
            cstr_args = ty_args;
            cstr_arity = List.length ty_args;
            cstr_tag = tag;
            cstr_consts = !num_consts;
            cstr_nonconsts = !num_nonconsts;
            cstr_normal = !num_normal;
            cstr_private = priv;
            cstr_generalized = ty_res_opt <> None
          } in
        (id, cstr) :: descr_rem in
  describe_constructors 0 0 cstrs

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
    cstr_generalized = false }

let extension_descr path_ext ext =
  let ty_res = 
    match ext.ext_ret_type with
	Some type_ret -> type_ret
      | None -> 
          newgenty (Tconstr(ext.ext_type_path, ext.ext_type_params, ref Mnil))
  in
  let tag =
    match ext.ext_args with
	[] -> Cstr_ext_constant (path_ext, ext.ext_loc)
      | _ -> Cstr_ext_block (path_ext, ext.ext_loc)
  in
  let existentials = 
    match ext.ext_ret_type with
      | None -> []
      | Some type_ret ->
	  let ret_vars = free_vars type_ret in
	  let arg_vars = free_vars (newgenty (Ttuple ext.ext_args)) in
	    TypeSet.elements (TypeSet.diff arg_vars ret_vars)
  in
    { cstr_name = Path.last path_ext;
      cstr_res = ty_res;
      cstr_existentials = existentials;
      cstr_args = ext.ext_args;
      cstr_arity = List.length ext.ext_args;
      cstr_tag = tag;
      cstr_consts = -1;
      cstr_nonconsts = -1;
      cstr_private = ext.ext_private;
      cstr_normal = -1;
      cstr_generalized = ext.ext_ret_type <> None }

let none = {desc = Ttuple []; level = -1; id = -1}
                                        (* Clearly ill-formed type *)
let dummy_label =
  { lbl_name = ""; lbl_res = none; lbl_arg = none; lbl_mut = Immutable;
    lbl_pos = (-1); lbl_all = [||]; lbl_repres = Record_regular;
    lbl_private = Public }

let label_descrs ty_res lbls repres priv =
  let all_labels = Array.create (List.length lbls) dummy_label in
  let rec describe_labels num = function
      [] -> []
    | (id, mut_flag, ty_arg) :: rest ->
        let lbl =
          { lbl_name = Ident.name id;
            lbl_res = ty_res;
            lbl_arg = ty_arg;
            lbl_mut = mut_flag;
            lbl_pos = num;
            lbl_all = all_labels;
            lbl_repres = repres;
            lbl_private = priv } in
        all_labels.(num) <- lbl;
        (id, lbl) :: describe_labels (num+1) rest in
  describe_labels 0 lbls

exception Constr_not_found

let rec find_constr tag num_const num_nonconst = function
    [] ->
      raise Constr_not_found
  | (name, ([] as cstr),(_ as ret_type_opt)) :: rem ->
      if tag = Cstr_constant num_const
      then (name,cstr,ret_type_opt)
      else find_constr tag (num_const + 1) num_nonconst rem
  | (name, (_ as cstr),(_ as ret_type_opt)) :: rem ->
      if tag = Cstr_block num_nonconst
      then (name,cstr,ret_type_opt)
      else find_constr tag num_const (num_nonconst + 1) rem

let find_constr_by_tag tag cstrlist =
  find_constr tag 0 0 cstrlist
