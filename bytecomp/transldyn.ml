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

open Asttypes
open Types
open Typedtree
open Longident
open Lambda

module OrderedInt = struct
  type t = int
  let compare = (-)
end
module IntMap = Map.Make(OrderedInt)

(* Get dynamics primitives identifiers *)
(* Adapted from Translobj.oo_prim. *)
let dynamics_prim name =
  try
    transl_path
      (fst (Env.lookup_value (Ldot (Lident "Dynamics", name)) Env.empty))
  with Not_found ->
    Misc.fatal_error ("Primitive Dynamics." ^ name ^ " not found.")

let dynamics_type loc name =
  let sty =
    { Parsetree.ptyp_desc =
        Parsetree.Ptyp_constr (Ldot (Lident "Dynamics", "type_data"), []);
      Parsetree.ptyp_loc = loc }
  in
  Typetexp.transl_simple_type Env.empty true sty


exception Unimplemented of string


let dummy_ty = {desc = Tvar; level = -1; id = -1}
let dummy_type_decl =
  { type_params = [];
    type_arity = 0;
    type_kind = Type_abstract;
    type_manifest = Some dummy_ty;
    type_variance = [] }

let extract_type_definitions whole_env ty0 =
  let r_env = ref Env.empty in
  let rec all ty = Btype.map_type_paths one ty
  and one path =
    let name = Path.name path in
    let id = Ident.create_persistent name in
    let path' = Path.Pident id in
    begin try
      let _ = Env.find_type path' !r_env in ()
    with Not_found ->
      let decl =
        Env.find_type path whole_env
      in
      r_env := Env.add_type id decl !r_env;
      let decl' =
        { type_params = List.map all decl.type_params;
          type_arity = decl.type_arity;
          type_kind = Type_abstract;
          type_manifest =
            begin match decl.type_manifest with
            | Some ty -> Some (all ty)
            | None ->
                match decl.type_kind with
                | Type_abstract ->
                    if Predef.is_predef_type_path path then None else
                    raise (Unimplemented
                             ("Dynamicisation involving an abstract type: " ^
                              name))
                | _ ->
                    raise (Unimplemented
                             ("Dynamicisation involving a generative type: " ^
                              name))
            end;
          type_variance = decl.type_variance }
      in
      r_env := Env.add_type id decl' !r_env;
    end;
    path'
  in
  !r_env, all ty0


(* From a type expression, produce code that builds a value that describes
   this type expression. Said value has the type Dynamics.type_bytes. *)
let make_type_repr_code whole_env ty0 =
  let ty1 = Ctype.correct_levels ty0 in
  Ctype.normalize_type whole_env ty1;
  let extracted_env, ty2 = extract_type_definitions whole_env ty1 in
  let s =
    Marshal.to_string (extracted_env, ty2 : Ctype.reified_type_data) []
  in
  Lconst (Const_base (Const_string s))


(* From a module type expression, produce code that builds a value that
   describes this module type expression. Said value has the type
   Dynamics.module_type_repr. *)
let make_sig_repr_code env mty0 =
  (*TODO*)
  Lconst (Const_pointer 0)
