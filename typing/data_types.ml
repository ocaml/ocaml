(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Gabriel Scherer, projet Picube, INRIA Paris                          *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Types

(* Constructor and record label descriptions inserted held in typing
   environments *)

type constructor_description =
  { cstr_name: string;                  (* Constructor name *)
    cstr_res: type_expr;                (* Type of the result *)
    cstr_existentials: type_expr list;  (* list of existentials *)
    cstr_args: type_expr list;          (* Type of the arguments *)
    cstr_arity: int;                    (* Number of arguments *)
    cstr_tag: constructor_tag;          (* Tag for heap blocks *)
    cstr_type_data: type_data option;   (* Type-global data, shared between all constructors.
                                           [None] for extensible types. *)
    cstr_generalized: bool;             (* Constrained return type? *)
    cstr_private: private_flag;         (* Read-only constructor? *)
    cstr_loc: Location.t;
    cstr_attributes: Parsetree.attributes;
    cstr_inlined: type_declaration option;
    cstr_uid: Uid.t;
   }

and constructor_tag =
    Cstr_constant of int                (* Constant constructor (an int) *)
  | Cstr_block of int                   (* Regular constructor (a block) *)
  | Cstr_unboxed of                     (* Constructor of an unboxed type *)
      Head_shape_types.unboxed_cstr_description
  | Cstr_extension of Path.t * bool     (* Extension constructor
                                           true if a constant false if a block*)

and type_data = {
  num_consts: int;              (* Number of constant constructors *)
  num_nonconsts: int;           (* Number of non-const constructors *)
  num_unboxed: int;             (* Number of unboxed constructors *)
  repr_data:                    (* Type-global representation information. *)
    (constructor_description list ref, repr_data) Misc.Cached.t;
    (* (It is computed on-demand from the list of constructors,
        as it depends on the environment and
        mutually-recursive type definitions). *)
}

and repr_data = {
  imm_stats: spread_data;
  tag_stats: spread_data;
}

and spread_data =
  | Any
  | Spread of {
      num: int; (* number of distinct values (immediates, tags) *)
      min: int; (* mimimal value (immediate, tag) *)
      max: int; (* maximal value (immediate, tag) *)
    }

let equal_constr cstr1 cstr2 =
  match cstr1.cstr_tag, cstr2.cstr_tag with
  | Cstr_constant i1, Cstr_constant i2 -> i2 = i1
  | Cstr_block i1, Cstr_block i2 -> i2 = i1
  | Cstr_unboxed _, Cstr_unboxed _ ->
      String.equal cstr1.cstr_name cstr2.cstr_name
  | Cstr_extension (path1, _), Cstr_extension (path2, _) ->
      Path.same path1 path2
  | (Cstr_constant _|Cstr_block _|Cstr_unboxed _|Cstr_extension _), _ ->
      false

let may_equal_constr c1 c2 =
  c1.cstr_arity = c2.cstr_arity
  && (match c1.cstr_tag,c2.cstr_tag with
     | Cstr_extension _, Cstr_extension _ ->
         (* extension constructors may be rebindings of each other *)
         true
     | (Cstr_extension _ | Cstr_constant _ | Cstr_block _ | Cstr_unboxed _),
       _ ->
         equal_constr c1 c2)

let cstr_res_type_path cstr =
  match get_desc cstr.cstr_res with
  | Tconstr (p, _, _) -> p
  | _ -> assert false

let cstr_res_type_params cstr =
  match get_desc cstr.cstr_res with
  | Tconstr (_, tyl, _) -> tyl
  | _ -> assert false

type label_description =
  { lbl_name: string;                   (* Short name *)
    lbl_res: type_expr;                 (* Type of the result (the record) *)
    lbl_arg: type_expr;                 (* Type of the argument
                                           (the field value) *)
    lbl_mut: mutable_flag;              (* Is this a mutable field? *)
    lbl_atomic: atomic_flag;            (* Is this an atomic field? *)
    lbl_pos: int;                       (* Position in block *)
    lbl_all: label_description array;   (* All the labels in this type *)
    lbl_repres: record_representation;  (* Representation for this record *)
    lbl_private: private_flag;          (* Read-only field? *)
    lbl_loc: Location.t;
    lbl_attributes: Parsetree.attributes;
    lbl_uid: Uid.t;
   }

let lbl_res_type_path lbl =
  match get_desc lbl.lbl_res with
  | Tconstr (p, _, _) -> p
  | _ -> assert false
