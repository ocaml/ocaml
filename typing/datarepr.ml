(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Compute constructor and label descriptions from type declarations,
   determining their representation. *)

open Misc
open Asttypes
open Types

module Duplicated_code = (* GAH : I'm duplicating this code from ctype *)
struct
  let rec free_vars ty =
    let ret = ref [] in
    let rec loop ty = 
      let ty = Btype.repr ty in
      if ty.level >= Btype.lowest_level then begin
	ty.level <- Btype.pivot_level - ty.level;
	match ty.desc with
	| Tvar ->
            ret := ty :: !ret
	| _ ->
	    Btype.iter_type_expr loop ty
      end
    in
    loop ty;
    Btype.unmark_type ty;
    !ret
end


let constructor_descrs_called = ref 0

let maybe_gadt_ty_res existentials ty_res = 
  match ty_res.desc with
  | Tconstr(_,lst,_) ->
      let contains_non_variables = 
	let is_not_variable t = 
	  match t.desc with
	  | Tvar -> false
	  | _ -> true 
	in
	List.length (List.filter is_not_variable lst) <> 0 
      in
      contains_non_variables || 
      List.length existentials <> 0 ||
      (let s = List.fold_right Btype.TypeSet.add  lst Btype.TypeSet.empty in 
      Btype.TypeSet.cardinal s <> List.length lst) 
    | _ -> assert false


let constructor_descrs ty_res cstrs priv =
  let num_consts = ref 0 and num_nonconsts = ref 0 in
  List.iter
    (function (name, [],_) -> incr num_consts
            | (name, _,_)  -> incr num_nonconsts)
    cstrs;
  let rec describe_constructors idx_const idx_nonconst = function
      [] -> []
    | (name, ty_args, ty_res_opt) :: rem ->
	let ty_res = 
	  match ty_res_opt with
	  | Some ty_res' -> 
	      (match ty_res.desc, ty_res'.desc with
	      | Tconstr (p,_,_), Tconstr(p',_,_) ->
		  if not (Path.same p p') then 
		    failwith "the return type of a generalized constructor has incorrect type" 
		  else
		    ty_res'
	      | _ -> fatal_error "return type must be a Tconstr")
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
		let res_vars = List.fold_right Btype.TypeSet.add (Duplicated_code.free_vars type_ret) Btype.TypeSet.empty in
		let arg_vars = 
		  List.fold_left 
		    (fun s list -> List.fold_right Btype.TypeSet.add list s)
		    Btype.TypeSet.empty 
		    (List.map Duplicated_code.free_vars ty_args)
		in
		Btype.TypeSet.elements (Btype.TypeSet.diff arg_vars res_vars)
	in
	incr constructor_descrs_called;
	let is_generalized = 
	  match ty_res_opt with
	    None -> false
	  | Some ty_res ->
	      maybe_gadt_ty_res existentials ty_res 
	in
	let cstr =
          { cstr_res = ty_res;    
	    cstr_existentials = existentials; 
            cstr_args = ty_args;
            cstr_arity = List.length ty_args;
            cstr_tag = tag;
            cstr_consts = !num_consts;
            cstr_nonconsts = !num_nonconsts;
	    cstr_normal = 0;
            cstr_private = priv;
	    cstr_generalized = is_generalized
	  } in
        (name, cstr) :: descr_rem in
  let ret = 
    describe_constructors 0 0 cstrs 
  in
  let normal = 
    List.length 
      (List.filter 
	 (fun (_,d) -> not (d.cstr_generalized)) 
	 ret) 
  in 
  List.map (fun (n,r) -> (n,{r with cstr_normal = normal})) ret

let exception_descr path_exc decl =
  { cstr_res = Predef.type_exn;
    cstr_existentials = [];
    cstr_args = decl;
    cstr_arity = List.length decl;
    cstr_tag = Cstr_exception path_exc;
    cstr_consts = -1;
    cstr_nonconsts = -1;
    cstr_private = Public;
    cstr_normal = -1;
    cstr_generalized = false }

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
    | (name, mut_flag, ty_arg) :: rest ->
        let lbl =
          { lbl_name = name;
            lbl_res = ty_res;
            lbl_arg = ty_arg;
            lbl_mut = mut_flag;
            lbl_pos = num;
            lbl_all = all_labels;
            lbl_repres = repres;
            lbl_private = priv } in
        all_labels.(num) <- lbl;
        (name, lbl) :: describe_labels (num+1) rest in
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
