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
open Primitive
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

let cache_required = ref false
let method_cache = Ident.create "*cache*"
let method_count = ref 0

let meth_tag s = Lconst(Const_base(Const_int(Btype.hash_variant s)))

let meth lab =
  let tag = meth_tag lab in
  if not (!cache_required && !Clflags.native_code) then (tag, []) else
  let n = !method_count in
  incr method_count;
  (tag, [Lprim(Pfield (2*n), [Lvar method_cache])])

let reset_labels () =
  Hashtbl.clear consts;
  method_count := 0

(* Insert labels *)

let string s = Lconst (Const_base (Const_string s))
let int n = Lconst (Const_base (Const_int n))

let prim_makearray =
  { prim_name = "make_vect"; prim_arity = 2; prim_alloc = true;
    prim_native_name = ""; prim_native_float = false }

let transl_label_init expr =
  let expr =
    Hashtbl.fold
      (fun c id expr -> Llet(Alias, id, Lconst c, expr))
      consts expr
  in
  let expr =
    if !method_count = 0 then expr else
    Llet(StrictOpt, method_cache,
         Lprim (Pccall prim_makearray, [int (2 * !method_count); int 0]),
         expr)
  in
  reset_labels ();
  expr


(* Share classes *)

let wrapping = ref false
let top_env = ref Env.empty
let classes = ref []

let oo_add_class id =
  classes := id :: !classes;
  (!top_env, !cache_required)

let oo_wrap env req f x =
  if !wrapping then
    if !cache_required then f x else
    try cache_required := true; let lam = f x in cache_required := false; lam
    with exn -> cache_required := false; raise exn
  else try
    wrapping := true;
    cache_required := req;
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
    
