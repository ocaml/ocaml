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
open Longident
open Lambda

(* Get oo primitives identifiers *)

let oo_prim name =
  try
    transl_path
      (fst (Env.lookup_value (Ldot (Lident "Oo", name)) Env.empty))
  with Not_found ->
    fatal_error ("Primitive " ^ name ^ " not found.")

(* Collect labels *)

let used_methods = ref ([] : (string * Ident.t) list);;

let meth lab =
  let lab = lab.Label.lab_name in
  try
    List.assoc lab !used_methods
  with Not_found ->
    let id = Ident.create lab in
    used_methods := (lab, id)::!used_methods;
    id

let reset_labels () =
  used_methods := []

(* Insert labels *)

let string s = Lconst (Const_base (Const_string s))

let transl_init kind labels expr =
  if labels = [] then
    expr
  else
    let init = Ident.create kind in
    Llet(Alias, init, oo_prim kind,
    List.fold_right
      (fun (lab, id) expr ->
         Llet(Strict, id, Lapply(Lvar init, [string lab]), expr))
      labels
      expr)

let transl_label_init expr =
  let new_method = Ident.create "new_method" in
  let expr' = transl_init "new_method"  !used_methods expr in
    reset_labels ();
    expr'
