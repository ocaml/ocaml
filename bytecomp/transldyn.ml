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
    fatal_error ("Primitive Dynamics." ^ name ^ " not found.")

let dynamics_type loc name =
  let sty =
    {ptyp_desc = Ptyp_constr (Ldot (Lident "Dynamics", "type_repr"), []);
     ptyp_loc = loc}
  in
  Typetexp.transl_simple_type Env.empty true sty


let canonicalize_type env ty0 =
  let rec traverse n vars ty =
    let ty' = Ctype.repr (Ctype.expand_head env ty) in
    match ty'.desc with
    | Tvar ->
        begin try
          n, vars, {ty' with id = IntMap.find ty'.id vars}
        with Not_found -> 
          n + 1, IntMap.add ty'.id n vars,
          {ty' with id = n}
        end
    | Tarrow (label, dom, im, commutable) ->
        let n, vars, dom' = traverse n vars dom in
        let n, vars, im' = traverse n vars im in
        n, vars, {ty' with desc = Tarrow (label, dom', im', commutable)}
    | Ttuple components ->
        let n, vars, components' = traverse_list n vars components in
        n, vars, {ty' with desc = Ttuple components'}
    | Tconstr (p, args, abbrev) ->
        let n, vars, args' = traverse_list n vars args in
        n, vars, {ty' with desc = Tconstr (p, args', abbrev)}
    | Tobject (ty1, r) ->
        let n, vars, ty1' = traverse n vars ty1 in
        n, vars, {ty' with desc = Tobject (ty1', r)}
    | Tfield (name, field_kind, ty1, ty2) ->
        let n, vars, ty1' = traverse n vars ty1 in
        let n, vars, ty2' = traverse n vars ty2 in
        n, vars, {ty' with desc = Tfield (name, field_kind, ty1', ty2')}
    | Tnil -> n, vars, {ty' with desc = Tnil}
    | Tlink ty1 -> traverse n vars ty1
    | Tsubst ty1 -> traverse n vars ty1
    | Tvariant row_desc ->
        assert(*TODO*)false
  and traverse_list n vars tys =
    List.fold_right
      (fun ty1 (n, vars, tail) ->
        let n, vars, ty1' = traverse n vars ty1 in
        n, vars, ty1' :: tail)
      tys (n, vars, [])
  in
  let n, _, ty = traverse 0 IntMap.empty ty0 in
  ty


let rec is_commutable = function
  | Cok -> true
  | Cunknown -> false
  | Clink {contents = c} -> is_commutable c

let cata_bool b =
  Const_pointer (if b then 1 else 0)

let cata_list lambdas =
  List.fold_right
    (fun x t -> Const_block (1, [x; t]))
    lambdas (Const_pointer 0)



exception Unimplemented of string

(* From a type expression, produce code that builds a value that describes
   this type expression. Said value has the type Dynamics.type_repr. *)
let make_type_repr_code env ty0 =
  (* Note that these constants have to match the definition of [type_repr] in
     stdlib/dynamics.ml *)
  let aardvark = ()
  and repr_tag_variable = 0
  and repr_tag_builtin = 1
  and repr_tag_tuple = 2
  and repr_tag_arrow = 3
  and repr_tag_variant = 4
  in
  let rec traverse ty =
    match ty.desc with
    | Tvar ->
        Const_block (repr_tag_variable,
                     [Const_base (Const_int ty.id)])
        (* FIXME: normalize the variable ids *)
    | Tarrow (label, dom, im, commutable) ->
        Const_block (repr_tag_arrow,
                     [Const_base (Const_string label);
                      traverse dom;
                      traverse im;
                      cata_bool (is_commutable commutable)])
    | Ttuple tys ->
        Const_block (repr_tag_tuple,
                     [cata_list (List.map traverse tys)])
    | Tconstr (p, tys, r) ->
        if Predef.is_predef_type_path p
        then
          Const_block (repr_tag_builtin,
                       [Const_base (Const_string (Path.name p));
                        cata_list (List.map traverse tys)])
        else
          Const_block (repr_tag_variant,
                       [Const_base (Const_string (Path.name p));
                        cata_list (List.map traverse tys)])
    | Tobject (ty1, r) ->
        raise (Unimplemented ("object"))
    | Tfield (name, field_kind, ty1, ty2) ->
        raise (Unimplemented ("field"))
    | Tnil ->
        raise (Unimplemented ("nil"))
    | Tlink ty1 -> traverse ty1
    | Tsubst ty1 -> traverse ty1
    | Tvariant row ->
        raise (Unimplemented ("row"))
  in
  let ty1 = canonicalize_type env ty0 in
  let z = Lconst (Const_block (1, [traverse ty1])) in
  z
