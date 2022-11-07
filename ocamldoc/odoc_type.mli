(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Cambium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Representation and manipulation of a type, but not class nor module type.*)

(** This module has an implementation although it declares only types.
    This is because other modules use the let module construct or access it
    so it is needed as a real module. *)

module Name = Odoc_name

type private_flag = Asttypes.private_flag = Private | Public

type record_field = {
  rf_name : string;
  rf_mutable : bool;
  rf_type : Types.type_expr;
  mutable rf_text : Odoc_types.info option;
}
(** Description of a record type field. *)

type constructor_args =
    Cstr_record of record_field list
  | Cstr_tuple of Types.type_expr list

type variant_constructor = {
  vc_name : string;
  vc_args : constructor_args;
  vc_ret : Types.type_expr option;
  mutable vc_text : Odoc_types.info option;
}
(** Description of a variant type constructor. *)

type type_kind =
    Type_abstract
  | Type_variant of variant_constructor list
  | Type_record of record_field list
  | Type_open
(** The various kinds of type. *)

type object_field = {
  of_name : string;
  of_type : Types.type_expr;
  mutable of_text : Odoc_types.info option;
}

type type_manifest =
    Other of Types.type_expr
  | Object_type of object_field list

type t_type = {
  ty_name : Name.t;
  mutable ty_info : Odoc_types.info option;
  ty_parameters : (Types.type_expr * bool * bool) list;
  ty_kind : type_kind;
  ty_private : private_flag;
  ty_manifest : type_manifest option;
  mutable ty_loc : Odoc_types.location;
  mutable ty_code : string option;
}

(** Representation of a type. *)
