(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
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
      (fst (Env.lookup_value (Ldot (Lident "CamlinternalOO", name)) Env.empty))
  with Not_found ->
    fatal_error ("Primitive " ^ name ^ " not found.")

(* Share blocks *)

let consts : (structured_constant, Ident.t) Hashtbl.t = Hashtbl.create 17

let share c =
  match c with
    Const_block (n, l) when l <> [] ->
      begin try
        Lvar (Hashtbl.find consts c)
      with Not_found ->
        let id = Ident.create "shared" in
        Hashtbl.add consts c id;
        Lvar id
      end
  | _ -> Lconst c

(* Collect labels *)

let used_methods = ref ([] : (string * Ident.t) list);;

let meth lab =
  try
    List.assoc lab !used_methods
  with Not_found ->
    let id = Ident.create lab in
    used_methods := (lab, id)::!used_methods;
    id

let reset_labels () =
  Hashtbl.clear consts;
  used_methods := []

(* Insert labels *)

let string s = Lconst (Const_base (Const_string s))

let transl_label_init expr =
  let expr =
    Hashtbl.fold
      (fun c id expr -> Llet(Alias, id, Lconst c, expr))
      consts expr
  in
  let expr =
    if !used_methods = [] then expr else
    let init = Ident.create "new_method" in
    Llet(StrictOpt, init, oo_prim "new_method",
         List.fold_right
           (fun (lab, id) expr ->
             Llet(StrictOpt, id, Lapply(Lvar init, [string lab]), expr))
           !used_methods
           expr)
  in
  reset_labels ();
  expr


(* Share classes *)

let wrapping = ref false
let required = ref true
let top_env = ref Env.empty
let classes = ref []

let oo_add_class id =
  classes := id :: !classes;
  (!top_env, !required)

let oo_wrap env req f x =
  if !wrapping then
    if !required then f x else
    try required := true; let lam = f x in required := false; lam
    with exn -> required := false; raise exn
  else try
    wrapping := true;
    required := req;
    top_env := env;
    classes := [];
    let lambda = f x in
    let lambda =
      List.fold_left
        (fun lambda id ->
          Llet(StrictOpt, id,
               Lprim(Pmakeblock(0, Mutable),
                     [lambda_unit; lambda_unit; lambda_unit]),
               lambda))
        lambda !classes
    in
    wrapping := false;
    top_env := Env.empty;
    lambda
  with exn ->
    wrapping := false;
    top_env := Env.empty;
    raise exn
    
