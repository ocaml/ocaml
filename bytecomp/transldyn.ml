(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Gilles Peskine, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Translation from typed abstract syntax to lambda terms,
   for dynamic typing *)

open Misc
open Longident
open Parsetree
open Asttypes
open Path
open Types
open Typedtree
open Lambda

(* Get dynamics primitives identifiers *)
(* Adapted from Translobj.oo_prim. *)
let dynamics_prim name =
  try
    transl_path
      (fst (Env.lookup_value (Ldot (Lident "Dynamics", name)) Env.empty))
  with Not_found ->
    fatal_error ("Primitive Dynamics." ^ name ^ " not found.")

let dynamics_type loc name =
  let sty =
    {ptyp_desc = Ptyp_constr (Ldot (Lident "Dynamics", "type_repr"), []);
     ptyp_loc = loc}
  in
  Typetexp.transl_simple_type Env.empty true sty

(* From a type expression, produce code that builds a value that describes
   this type expression. Said value has the type Dynamics.type_repr. *)
let make_type_repr_code ty =
  (* Note that these constants have to match the definition of [type_repr] in
     stdlib/dynamics.ml *)
  let aardvark = ()
  in
  let rec traverse ty =
    match ty.desc with
    | Tvar -> Const_base (Const_int 0)
    | Tarrow (label, dom, im, commutable) ->
        Const_base (Const_int 1)
    | Ttuple tys -> Const_base (Const_int 2)
    | Tconstr (name, tys, r) -> Const_base (Const_int 3)
    | Tobject (ty1, r) -> Const_base (Const_int 4)
    | Tfield (name, field_kind, ty1, ty2) -> Const_base (Const_int 5)
    | Tnil -> Const_base (Const_int 6)
    | Tlink ty1 -> traverse ty1
    | Tsubst ty1 -> traverse ty1
    | Tvariant row -> Const_base (Const_int 7)
  in
  Lconst (Const_block (1, [traverse ty]))

(*
let type_expr loc te =
  {exp_desc = make_type_repr_code te;
   exp_loc = loc;
   exp_type = dynamics_type loc "type_repr";
   exp_env = Env.empty};
*)
let type_expr loc te = make_type_repr_code te
