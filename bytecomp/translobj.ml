(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
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
        mk_lam (Lvar (Hashtbl.find consts c))
      with Not_found ->
        let id = Ident.create "shared" in
        Hashtbl.add consts c id;
        mk_lam (Lvar id)
      end
  | _ -> mk_lam (Lconst c)

(* Collect labels *)

let cache_required = ref false
let method_cache = ref lambda_unit
let method_count = ref 0
let method_table = ref []

let meth_tag s = mk_lam (Lconst(Const_base(Const_int(Btype.hash_variant s))))

let next_cache tag =
  let n = !method_count in
  incr method_count;
  (tag, [!method_cache; mk_lam (Lconst(Const_base(Const_int n)))])

let rec is_path l = match l.l_desc with
    Lvar _ | Lprim (Pgetglobal _, []) | Lconst _ -> true
  | Lprim (Pfield _, [lam]) -> is_path lam
  | Lprim ((Parrayrefu _ | Parrayrefs _), [lam1; lam2]) ->
      is_path lam1 && is_path lam2
  | _ -> false

let meth obj lab =
  let tag = meth_tag lab in
  if not (!cache_required && !Clflags.native_code) then (tag, []) else
  if not (is_path obj) then next_cache tag else
  try
    (* let r = List.assoc obj.l_desc !method_table in *)
    let rec find obj = function
      | [] -> raise Not_found
      | (lam, lams) :: tl ->  if same lam obj then lams else find obj tl in
    let r = find obj !method_table in
    try
      (tag, find tag !r)(* List.assoc tag !r *)
    with Not_found ->
      let p = next_cache tag in
      r := p :: !r;
      p
  with Not_found ->
    let p = next_cache tag in
    method_table := (obj, ref [p]) :: !method_table;
    p

let reset_labels () =
  Hashtbl.clear consts;
  method_count := 0;
  method_table := []

(* Insert labels *)

let string s = mk_lam (Lconst (Const_base (Const_string s)))
let int n = mk_lam (Lconst (Const_base (Const_int n)))

let prim_makearray =
  { prim_name = "caml_make_vect"; prim_arity = 2; prim_alloc = true;
    prim_native_name = ""; prim_native_float = false }

let transl_label_init expr =
  let expr =
    Hashtbl.fold
      (fun c id expr -> mk_lam (Llet(Alias, id, mk_lam (Lconst c), expr)))
      consts expr
  in
  reset_labels ();
  expr

let transl_store_label_init glob size f arg =
  method_cache := mk_lam (Lprim(Pfield size, [mk_lam (Lprim(Pgetglobal glob, []))]));
  let expr = f arg in
  let (size, expr) =
    if !method_count = 0 then (size, expr) else
      (size+1,
       mk_lam (Lsequence(
         mk_lam (Lprim(Psetfield(size, false),
                       [mk_lam (Lprim(Pgetglobal glob, []));
                        mk_lam (Lprim (Pccall prim_makearray, [int !method_count; int 0]))])),
         expr)))
  in
  (size, transl_label_init expr)

(* Share classes *)

let wrapping = ref false
let top_env = ref Env.empty
let classes = ref []
let method_ids = ref IdentSet.empty

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
         method_ids := IdentSet.empty;
         let l = f x in
         let lambda =
           List.fold_left
             (fun expr id ->
               mk_lam (Llet(StrictOpt, id,
                            mk_lam (Lprim(Pmakeblock(0, Mutable),
                                          [lambda_unit; lambda_unit; lambda_unit])),
                            expr)))
             l !classes
         in
         wrapping := false;
         top_env := Env.empty;
         lambda
    with exn ->
      wrapping := false;
      top_env := Env.empty;
      raise exn
