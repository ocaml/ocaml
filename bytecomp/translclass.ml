(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Misc
open Asttypes
open Typedtree
open Lambda
open Translobj
open Translcore


let transl_label l = Lconst (Const_base (Const_string l))

(* Instance variable initialization *)
let set_inst_var obj var id expr =
  Lprim(Parraysetu (if maybe_pointer expr then Paddrarray else Pintarray),
        [Lvar obj; Lvar id; transl_exp expr])


let transl_super tbl inh_methods rem =
  List.fold_right
    (fun (nm, id) rem ->
       Llet(StrictOpt, id, Lapply (oo_prim "get_method",
		                   [Lvar tbl; Lvar (meth nm)]),
       rem))
    inh_methods rem

let transl_val tbl name id rem =
  Llet(StrictOpt, id, Lapply (oo_prim "get_variable",
		              [Lvar tbl; transl_label name]),
       rem)

let transl_private_val tbl name id rem =
  Llet(StrictOpt, id, Lapply (oo_prim "get_private_variable",
		              [Lvar tbl; transl_label name]),
       rem)

let transl_vals tbl vals rem =
  List.fold_right
    (fun (name, id) rem -> transl_val tbl name id rem)
    vals rem

let inherited_values vals =
  Lconst
    (List.fold_right
       (fun (v, _) rem ->
          Const_block(0, [Const_base (Const_string v); rem]))
       vals (Const_pointer 0))

let transl_field_obj obj field (obj_init, anc_id) =
  match field with
    Cf_inher (name, args, vals, meths) ->
      let init = Ident.create "init" in
      (Lsequence(Lapply(Lvar init, Lvar obj :: (List.map transl_exp args)),
		 obj_init),
       init::anc_id)
  | Cf_val (name, id, priv, Some exp) ->
      (Lsequence(set_inst_var obj name id exp, obj_init),
       anc_id)
  | Cf_val (name, id, priv, None) ->
      (obj_init, anc_id)
  | Cf_meth (name, exp) ->
      (obj_init, anc_id)

let transl_field_cl tbl field cl_init =
  match field with
    Cf_inher (name, args, vals, meths) ->
      Lsequence(Lapply (oo_prim "inheritance", [Lvar tbl; transl_path name;
                                                inherited_values vals]),
      	       	transl_vals tbl vals (
                transl_super tbl meths cl_init))
  | Cf_val (name, id, priv, exp) ->
      if priv = Private then
        transl_private_val tbl name id cl_init
      else
        transl_val tbl name id cl_init
  | Cf_meth (name, exp) ->
      Lsequence(Lapply (oo_prim "set_method",
                        [Lvar tbl; Lvar (meth name); transl_exp exp]),
		cl_init)

let transl_val_hiding tbl cl_init =
  function
    Cf_inher _ | Cf_meth _ | Cf_val (_, _, Public, _) ->
      cl_init
  | Cf_val (name, id, Private, exp) ->
      Lsequence(Lapply (oo_prim "hide_variable",
                        [Lvar tbl; transl_label name]),
		cl_init)

let transl_class cl =
  let obj = Ident.create "obj" in
  let (field_init, anc_id) =
    List.fold_right (transl_field_obj obj) cl.cl_field (Lvar obj, [])
  in
  let (params, body) =
    List.fold_right
      (fun pat (params, rem) ->
        let param = name_pattern "param" [pat, ()] in
        (param::params,
         Matching.for_function pat.pat_loc (Lvar param) [pat, rem]))
      cl.cl_args
      ([], field_init)
  in
  let obj_init = Lfunction(anc_id @ obj::params, body) in
  let table = Ident.create "table" in
  let cl_init =
    Lfunction ([table],
               List.fold_left (transl_val_hiding table)
                 (List.fold_right (transl_field_cl table) cl.cl_field
	            (Lapply (oo_prim "set_initializer",
                             [Lvar table; obj_init])))
                 cl.cl_field)
  in
    Lapply (oo_prim "create_class", [cl_init])
