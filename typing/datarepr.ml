(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Compute constructor and label descriptions from type declarations,
   determining their representation. *)

open Misc
open Asttypes
open Types

let constructor_descrs ty_res cstrs =
  let num_consts = ref 0 and num_nonconsts = ref 0 in
  List.iter
    (function (name, []) -> incr num_consts
            | (name, _)  -> incr num_nonconsts)
    cstrs;
  let rec describe_constructors idx_const idx_nonconst = function
      [] -> []
    | (name, ty_args) :: rem ->
        let (tag, descr_rem) =
          match ty_args with
            [] -> (Cstr_constant idx_const,
                   describe_constructors (idx_const+1) idx_nonconst rem)
          | _  -> (Cstr_block idx_nonconst,
                   describe_constructors idx_const (idx_nonconst+1) rem) in
        let cstr =
          { cstr_res = ty_res;
            cstr_args = ty_args;
            cstr_arity = List.length ty_args;
            cstr_tag = tag;
            cstr_consts = !num_consts;
            cstr_nonconsts = !num_nonconsts } in
        (name, cstr) :: descr_rem in
  describe_constructors 0 0 cstrs

let exception_descr path_exc decl =
  { cstr_res = Predef.type_exn;
    cstr_args = decl;
    cstr_arity = List.length decl;
    cstr_tag = Cstr_exception path_exc;
    cstr_consts = -1;
    cstr_nonconsts = -1 }

let none = {desc = Ttuple []; level = -1; id = -1}
                                        (* Clearly ill-formed type *)
let dummy_label =
  { lbl_res = none; lbl_arg = none; lbl_mut = Immutable;
    lbl_pos = (-1); lbl_all = [||]; lbl_repres = Record_regular }

(* Cannot call ctype.repres here *)

let rec is_float = 
  function
    {desc = Tlink ty} -> is_float ty
  | {desc = Tconstr(p, _, _)} -> Path.same p Predef.path_float
  | _ -> false

let label_descrs ty_res lbls =
  let all_labels = Array.create (List.length lbls) dummy_label in
  let repres =
    if List.for_all (fun (name, flag, ty) -> is_float ty) lbls
    then Record_float
    else Record_regular in
  let rec describe_labels num = function
      [] -> []
    | (name, mut_flag, ty_arg) :: rest ->
        let lbl =
          { lbl_res = ty_res;
            lbl_arg = ty_arg;
            lbl_mut = mut_flag;
            lbl_pos = num;
            lbl_all = all_labels;
            lbl_repres = repres } in
        all_labels.(num) <- lbl;
        (name, lbl) :: describe_labels (num+1) rest in
  describe_labels 0 lbls
