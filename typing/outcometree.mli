(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Module [Outcometree]: results displayed by the toplevel *)

(* These types represent messages that the toplevel displays as normal
   results or errors. The real displaying is customisable using the hooks:
      [Toploop.print_out_value]
      [Toploop.print_out_type]
      [Toploop.print_out_sig_item]
      [Toploop.print_out_phrase] *)

type out_string =
  | Ostr_string
  | Ostr_bytes

type out_attribute =
  { oattr_name: string }

type out_rec_status =
  | Orec_not
  | Orec_first
  | Orec_next
type out_ext_status =
  | Oext_first
  | Oext_next
  | Oext_exception


module type S = sig
  type 'a ext

  type out_ident =
    | Oide_apply of out_ident ext * out_ident ext
    | Oide_dot of out_ident ext * string ext
    | Oide_ident of string ext

  type out_value =
    | Oval_array of out_value list
    | Oval_char of char
    | Oval_constr of out_ident * out_value list
    | Oval_ellipsis
    | Oval_float of float
    | Oval_int of int
    | Oval_int32 of int32
    | Oval_int64 of int64
    | Oval_nativeint of nativeint
    | Oval_list of out_value list
    | Oval_printer of (Format.formatter -> unit)
    | Oval_record of (out_ident * out_value) list
    | Oval_string of string * int * out_string (* string, size-to-print, kind *)
    | Oval_stuff of string
    | Oval_tuple of out_value list
    | Oval_variant of string * out_value option


  type out_type =
    | Otyp_abstract
    | Otyp_open
    | Otyp_alias of out_type ext * string ext
    | Otyp_arrow of out_labelled ext * out_type ext
    | Otyp_class of bool ext * out_ident ext * out_type ext list
    | Otyp_constr of out_ident ext * out_type ext list
    | Otyp_manifest of out_type ext * out_type ext
    | Otyp_object of out_labelled ext list * bool ext option ext
    | Otyp_record of out_field ext list
    | Otyp_stuff of string
    | Otyp_sum of out_constructor ext list
    | Otyp_tuple of out_type ext list
    | Otyp_var of bool ext * string ext
    | Otyp_variant of
        bool ext * out_variant ext * bool ext * string ext list option ext
    | Otyp_poly of string ext list * out_type ext
    | Otyp_module of string ext * string ext list * out_type ext list
    | Otyp_attribute of out_type ext * out_attribute ext

  and out_labelled = (string ext * out_type ext)

  and out_constructor =
    {cname:string ext; args:out_type ext list; ret:out_type ext option ext}

  and out_variant =
    | Ovar_fields of out_var_field ext list
    | Ovar_typ of out_type ext

  and out_var_field = {tag:string ext; ampersand:bool ext; conj: out_type ext list}

  and out_field = {label:string ext; mut: bool ext; typ:out_type ext}

  type type_constraint = {lhs:out_type ext ;rhs:out_type ext}

  type out_class_type =
    | Octy_constr of out_ident ext * out_type ext list
    | Octy_arrow of out_labelled ext * out_class_type ext
    | Octy_signature of out_type ext option ext * out_class_sig_item ext list
  and out_class_sig_item =
    | Ocsg_constraint of type_constraint
    | Ocsg_method of string ext * bool ext * bool ext * out_type ext
    | Ocsg_value of string ext * bool ext * bool ext * out_type ext

  type type_param = {covariant:bool ext;contravariant:bool ext;name:string ext}

  type out_module_type =
    | Omty_abstract
    | Omty_functor of
        functor_arg ext * out_module_type ext
    | Omty_ident of out_ident ext
    | Omty_signature of out_sig_item ext list
    | Omty_alias of out_ident ext
  and functor_arg = string ext * out_module_type ext option ext
  and out_sig_item =
    | Osig_class of
        bool ext * string ext * type_param ext list * out_class_type ext
        * out_rec_status ext
    | Osig_class_type of
        bool ext * string ext * type_param ext list * out_class_type ext
        * out_rec_status ext
    | Osig_typext of out_extension_constructor * out_ext_status ext
    | Osig_modtype of string ext * out_module_type ext
    | Osig_module of string ext * out_module_type ext * out_rec_status ext
    | Osig_type of out_type_decl * out_rec_status ext
    | Osig_value of out_val_decl
    | Osig_ellipsis
  and out_type_decl =
    { otype_name: string ext;
      otype_params: type_param ext list;
      otype_type: out_type ext;
      otype_private: Asttypes.private_flag ext;
      otype_immediate: bool ext;
      otype_unboxed: bool ext;
      otype_cstrs: type_constraint ext list }
  and out_extension_constructor =
    { oext_name: string ext;
      oext_type_name: string ext;
      oext_type_params: string ext list;
      oext_args: out_type ext list;
      oext_ret_type: out_type ext option ext;
      oext_private: Asttypes.private_flag ext }
  and out_type_extension =
    { otyext_name: string ext;
      otyext_params: string ext list;
      otyext_constructors: out_constructor ext list;
      otyext_private: Asttypes.private_flag ext }
  and out_val_decl =
    { oval_name: string ext;
      oval_type: out_type ext;
      oval_prims: string ext list;
      oval_attributes: out_attribute ext list }

  type out_phrase =
    | Ophr_eval of out_value * out_type
    | Ophr_signature of (out_sig_item * out_value option) list
    | Ophr_exception of (exn * out_value)
end

include S with type 'a ext = 'a

module Highlightable: sig
  type emphase = On | Off
  type 'a t =
    | Item of emphase * 'a
    | Ellipsis of int (** ellipsis length *)
end
module Decorated: sig   include S with type 'a ext = 'a Highlightable.t end

module Decorate:sig
  val value: ?highlight:Highlightable.emphase
    -> out_value -> Decorated.out_value Decorated.ext
  val ident: ?highlight:Highlightable.emphase
    ->  out_ident -> Decorated.out_ident Decorated.ext
  val typ: ?highlight:Highlightable.emphase
    ->  out_type -> Decorated.out_type Decorated.ext
  val variant: ?highlight:Highlightable.emphase
    -> out_variant -> Decorated.out_variant Decorated.ext
  val type_extension: ?highlight:Highlightable.emphase
    -> out_type_extension ->
    Decorated.out_type_extension Decorated.ext
  val sig_item: ?highlight:Highlightable.emphase
    -> out_sig_item -> Decorated.out_sig_item Decorated.ext
  val signature: ?highlight:Highlightable.emphase
    -> out_sig_item list ->
    Decorated.out_sig_item Decorated.ext list
  val module_type: ?highlight:Highlightable.emphase
    -> out_module_type -> Decorated.out_module_type Decorated.ext
  val class_type: ?highlight:Highlightable.emphase
    ->out_class_type -> Decorated.out_class_type Decorated.ext
  val phrase: ?highlight:Highlightable.emphase
    -> out_phrase -> Decorated.out_phrase
end
