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


let no_abbrev = ref Mnil
let canonicalize_type env ty0 =
  let n = ref 0 and vars = ref IntMap.empty in
  let rec traverse ty =
    let ty' = Ctype.repr (Ctype.expand_head env ty) in
    match ty'.desc with
    | Tvar ->
        begin try
          {ty' with id = IntMap.find ty'.id !vars}
        with Not_found ->
          let n1 = !n in
          incr n;
          vars := IntMap.add ty'.id n1 !vars;
          {ty' with id = n1}
        end
    | Tarrow (label, dom, im, commutable) ->
        let dom' = traverse dom in
        let im' = traverse im in
        {ty' with desc = Tarrow (label, dom', im', commutable)}
    | Ttuple components ->
        let components' = traverse_list components in
        {ty' with desc = Ttuple components'}
    | Tconstr (p, args, abbrev) ->
        let args' = traverse_list args in
        {ty' with desc = Tconstr (p, args', no_abbrev)}
    | Tobject (ty1, {contents = o}) ->
        let ty1' = traverse ty1 in
        let o' = match o with
        | None -> None
        | Some (p, tys) -> Some (p, traverse_list tys)
        in
        {ty' with desc = Tobject (ty1', ref o')}
    | Tfield (name, field_kind, ty1, ty2) ->
        let ty1' = traverse ty1 in
        let ty2' = traverse ty2 in
        {ty' with desc = Tfield (name, field_kind, ty1', ty2')}
    | Tnil -> ty'
    | Tlink ty1 -> traverse ty1
    | Tsubst ty1 -> traverse ty1
    | Tvariant r ->
        let fields' =
          let t = ref [] in
          List.iter
            (fun (label, field1) ->
              t := (label, traverse_field field1) :: !t)
            r.row_fields;
          !t
        in
        let more' = traverse r.row_more in
        let bound' = traverse_list r.row_bound in
        let name' = match r.row_name with
        | None -> None
        | Some (p, tys) -> Some (p, traverse_list tys)
        in
        {ty' with desc = Tvariant {r with
                                   row_fields = fields';
                                   row_more = more';
                                   row_bound = bound';
                                   row_name = name'}}
  and traverse_list tys =
    let t = ref [] in
    List.iter (fun ty -> t := traverse ty :: !t) tys;
    !t
  and traverse_field = function
    | Rpresent (Some ty1) -> Rpresent (Some (traverse ty1))
    | Reither (c, tys, b, {contents = o}) ->
        let tys' = traverse_list tys in
        let o' = match o with
        | None -> None
        | Some field2 -> Some (traverse_field field2)
        in
        Reither (c, tys', b, ref o')
    | field -> field
  in
  traverse ty0


let rec is_commutable = function
  | Cok -> true
  | Cunknown -> false
  | Clink {contents = c} -> is_commutable c

(* Note that these constants have to match the definitions of the type
   [type_repr] in stdlib/dynamics.ml, as well as those of [list] and [option],
   and the indexing algorithm for classical variants. *)
let cata_nil = Const_pointer 0
and cata_cons h t = Const_block (0, [h; t])
let cata_none = Const_pointer 0
and cata_some x = Const_block (0, [x])
let repr_tag_variable = 0
and repr_tag_builtin = 1
and repr_tag_tuple = 2
and repr_tag_arrow = 3
and repr_tag_variant = 4
and repr_tag_classical_variant = 5
let repr_tag_row_present = 0
and repr_tag_row_either = 1
and repr_tag_row_absent = 2

let cata_bool b = Const_pointer (if b then 1 else 0)
let cata_list lambdas =
  List.fold_right cata_cons lambdas (Const_pointer 0)


exception Unimplemented of string

(* From a type expression, produce code that builds a value that describes
   this type expression. Said value has the type Dynamics.type_repr. *)
let make_type_repr_code env ty0 =
  let rec traverse ty =
    match ty.desc with
    | Tvar ->
        Const_block (repr_tag_variable,
                     [Const_base (Const_int ty.id)])
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
          Const_block (repr_tag_classical_variant,
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
        Const_block (repr_tag_variant,
                     [traverse_row row])
  and traverse_row row =
    Const_block (0,
                 [cata_list (List.map traverse_labeled_row_field
                                      row.row_fields);
                  traverse row.row_more;
                  cata_list (List.map traverse row.row_bound);
                  cata_bool row.row_closed;
                  traverse_row_name row.row_name])
  and traverse_row_field = function
    | Rpresent None ->
        Const_block (repr_tag_row_present, [cata_none])
    | Rpresent (Some ty) ->
        Const_block (repr_tag_row_present, [cata_some (traverse ty)])
    | Reither (is_const, tys, b, {contents = o}) ->
        let o_code = match o with
        | None -> cata_none
        | Some row_field -> cata_some (traverse_row_field row_field)
        in
        Const_block (repr_tag_row_either,
                     [cata_bool is_const;
                      cata_list (List.map traverse tys);
                      cata_bool b;
                      o_code])
    | Rabsent -> Const_pointer repr_tag_row_absent
  and traverse_labeled_row_field (label, field) =
    Const_block (0, [Const_base (Const_string label);
                     traverse_row_field field])
  and traverse_row_name = function
    | None -> cata_none
    | Some (p, tys) ->
        cata_some
          (Const_block (0, [Const_base (Const_string (Path.name p));
                            cata_list (List.map traverse tys)]))
  in
  Ctype.normalize_type env ty0;
  let ty1 = canonicalize_type env ty0 in
  let z = Lconst (Const_block (0, [traverse ty1])) in
  z


(* From a module type expression, produce code that builds a value that
   describes this module type expression. Said value has the type
   Dynamics.module_type_repr. *)
let make_sig_repr_code env mty0 =
  (*TODO*)
  Lconst (Const_pointer 0)
