(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                        Alain Frisch, LexiFi                         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Helpers to produce Parsetree fragments *)

open Parsetree
open Asttypes

module Typ :
  sig
    val mk: ?attrs:attribute list -> ?loc:Location.t -> core_type_desc -> core_type
    val attr: core_type -> attribute -> core_type

    val any: ?loc:Location.t -> ?attrs:attribute list -> unit -> core_type
    val var: ?loc:Location.t -> ?attrs:attribute list -> string -> core_type
    val arrow: ?loc:Location.t -> ?attrs:attribute list -> label -> core_type -> core_type -> core_type
    val tuple: ?loc:Location.t -> ?attrs:attribute list -> core_type list -> core_type
    val constr: ?loc:Location.t -> ?attrs:attribute list -> Longident.t loc -> core_type list -> core_type
    val object_: ?loc:Location.t -> ?attrs:attribute list -> core_field_type list -> core_type
    val class_: ?loc:Location.t -> ?attrs:attribute list -> Longident.t loc -> core_type list -> label list -> core_type
    val alias: ?loc:Location.t -> ?attrs:attribute list -> core_type -> string -> core_type
    val variant: ?loc:Location.t -> ?attrs:attribute list -> row_field list -> bool -> label list option -> core_type
    val poly: ?loc:Location.t -> ?attrs:attribute list -> string list -> core_type -> core_type
    val package: ?loc:Location.t -> ?attrs:attribute list -> Longident.t loc -> (Longident.t loc * core_type) list -> core_type
    val extension: ?loc:Location.t -> ?attrs:attribute list -> extension -> core_type
  end
module Pat:
  sig
    val mk: ?attrs:attribute list -> ?loc:Location.t -> pattern_desc -> pattern
    val attr:pattern -> attribute -> pattern
    val any: ?loc:Location.t -> ?attrs:attribute list -> unit -> pattern
    val var: ?loc:Location.t -> ?attrs:attribute list -> string loc -> pattern
    val alias: ?loc:Location.t -> ?attrs:attribute list -> pattern -> string loc -> pattern
    val constant: ?loc:Location.t -> ?attrs:attribute list -> constant -> pattern
    val tuple: ?loc:Location.t -> ?attrs:attribute list -> pattern list -> pattern
    val construct: ?loc:Location.t -> ?attrs:attribute list -> Longident.t loc -> pattern option -> bool -> pattern
    val variant: ?loc:Location.t -> ?attrs:attribute list -> label -> pattern option -> pattern
    val record: ?loc:Location.t -> ?attrs:attribute list -> (Longident.t loc * pattern) list -> closed_flag -> pattern
    val array: ?loc:Location.t -> ?attrs:attribute list -> pattern list -> pattern
    val or_: ?loc:Location.t -> ?attrs:attribute list -> pattern -> pattern -> pattern
    val constraint_: ?loc:Location.t -> ?attrs:attribute list -> pattern -> core_type -> pattern
    val type_: ?loc:Location.t -> ?attrs:attribute list -> Longident.t loc -> pattern
    val lazy_: ?loc:Location.t -> ?attrs:attribute list -> pattern -> pattern
    val unpack: ?loc:Location.t -> ?attrs:attribute list -> string loc -> pattern
    val extension: ?loc:Location.t -> ?attrs:attribute list -> extension -> pattern
  end
module Exp:
  sig
    val mk: ?attrs:attribute list -> ?loc:Location.t -> expression_desc -> expression
    val attr: expression -> attribute -> expression
    val ident: ?loc:Location.t -> ?attrs:attribute list -> Longident.t loc -> expression
    val constant: ?loc:Location.t -> ?attrs:attribute list -> constant -> expression
    val let_: ?loc:Location.t -> ?attrs:attribute list -> rec_flag -> (pattern * expression) list -> expression -> expression
    val function_: ?loc:Location.t -> ?attrs:attribute list -> label -> expression option -> (pattern * expression) list -> expression
    val apply: ?loc:Location.t -> ?attrs:attribute list -> expression -> (label * expression) list -> expression
    val match_: ?loc:Location.t -> ?attrs:attribute list -> expression -> (pattern * expression) list -> expression
    val try_: ?loc:Location.t -> ?attrs:attribute list -> expression -> (pattern * expression) list -> expression
    val tuple: ?loc:Location.t -> ?attrs:attribute list -> expression list -> expression
    val construct: ?loc:Location.t -> ?attrs:attribute list -> Longident.t loc -> expression option -> bool -> expression
    val variant: ?loc:Location.t -> ?attrs:attribute list -> label -> expression option -> expression
    val record: ?loc:Location.t -> ?attrs:attribute list -> (Longident.t loc * expression) list -> expression option -> expression
    val field: ?loc:Location.t -> ?attrs:attribute list -> expression -> Longident.t loc -> expression
    val setfield: ?loc:Location.t -> ?attrs:attribute list -> expression -> Longident.t loc -> expression -> expression
    val array: ?loc:Location.t -> ?attrs:attribute list -> expression list -> expression
    val ifthenelse: ?loc:Location.t -> ?attrs:attribute list -> expression -> expression -> expression option -> expression
    val sequence: ?loc:Location.t -> ?attrs:attribute list -> expression -> expression -> expression
    val while_: ?loc:Location.t -> ?attrs:attribute list -> expression -> expression -> expression
    val for_: ?loc:Location.t -> ?attrs:attribute list -> string loc -> expression -> expression -> direction_flag -> expression -> expression
    val constraint_: ?loc:Location.t -> ?attrs:attribute list -> expression -> core_type option -> core_type option -> expression
    val when_: ?loc:Location.t -> ?attrs:attribute list -> expression -> expression -> expression
    val send: ?loc:Location.t -> ?attrs:attribute list -> expression -> string -> expression
    val new_: ?loc:Location.t -> ?attrs:attribute list -> Longident.t loc -> expression
    val setinstvar: ?loc:Location.t -> ?attrs:attribute list -> string loc -> expression -> expression
    val override: ?loc:Location.t -> ?attrs:attribute list -> (string loc * expression) list -> expression
    val letmodule: ?loc:Location.t -> ?attrs:attribute list -> string loc -> module_expr -> expression -> expression
    val assert_: ?loc:Location.t -> ?attrs:attribute list -> expression -> expression
    val assertfalse: ?loc:Location.t -> ?attrs:attribute list -> unit -> expression
    val lazy_: ?loc:Location.t -> ?attrs:attribute list -> expression -> expression
    val poly: ?loc:Location.t -> ?attrs:attribute list -> expression -> core_type option -> expression
    val object_: ?loc:Location.t -> ?attrs:attribute list -> class_structure -> expression
    val newtype: ?loc:Location.t -> ?attrs:attribute list -> string -> expression -> expression
    val pack: ?loc:Location.t -> ?attrs:attribute list -> module_expr -> expression
    val open_: ?loc:Location.t -> ?attrs:attribute list -> Longident.t loc -> expression -> expression
    val extension: ?loc:Location.t -> ?attrs:attribute list -> extension -> expression
  end
module Mty:
  sig
    val mk: ?attrs:attribute list -> ?loc:Location.t -> module_type_desc -> module_type
    val attr: module_type -> attribute -> module_type
    val ident: ?loc:Location.t -> ?attrs:attribute list -> Longident.t loc -> module_type
    val signature: ?loc:Location.t -> ?attrs:attribute list -> signature -> module_type
    val functor_: ?loc:Location.t -> ?attrs:attribute list -> string loc -> module_type -> module_type -> module_type
    val with_: ?loc:Location.t -> ?attrs:attribute list -> module_type -> (Longident.t loc * with_constraint) list -> module_type
    val typeof_: ?loc:Location.t -> ?attrs:attribute list -> module_expr -> module_type
    val extension: ?loc:Location.t -> ?attrs:attribute list -> extension -> module_type
  end
module Mod:
  sig
    val mk: ?attrs:attribute list -> ?loc:Location.t -> module_expr_desc -> module_expr
    val attr: module_expr -> attribute -> module_expr
    val ident: ?loc:Location.t -> ?attrs:attribute list -> Longident.t loc -> module_expr
    val structure: ?loc:Location.t -> ?attrs:attribute list -> structure -> module_expr
    val functor_: ?loc:Location.t -> ?attrs:attribute list -> string loc -> module_type -> module_expr -> module_expr
    val apply: ?loc:Location.t -> ?attrs:attribute list -> module_expr -> module_expr -> module_expr
    val constraint_: ?loc:Location.t -> ?attrs:attribute list -> module_expr -> module_type -> module_expr
    val unpack: ?loc:Location.t -> ?attrs:attribute list -> expression -> module_expr
    val extension: ?loc:Location.t -> ?attrs:attribute list -> extension -> module_expr
  end
module Sig:
  sig
    val mk: ?loc:Location.t -> signature_item_desc -> signature_item
    val value: ?loc:Location.t -> value_description -> signature_item
    val type_: ?loc:Location.t -> type_declaration list -> signature_item
    val exception_: ?loc:Location.t -> constructor_declaration -> signature_item
    val module_: ?loc:Location.t -> module_declaration -> signature_item
    val rec_module: ?loc:Location.t -> module_declaration list -> signature_item
    val modtype: ?loc:Location.t -> module_type_declaration -> signature_item
    val open_: ?loc:Location.t -> ?attrs:attribute list -> Longident.t loc -> signature_item
    val include_: ?loc:Location.t -> ?attrs:attribute list -> module_type -> signature_item
    val class_: ?loc:Location.t -> class_description list -> signature_item
    val class_type: ?loc:Location.t -> class_type_declaration list -> signature_item
    val extension: ?loc:Location.t -> ?attrs:attribute list -> extension -> signature_item
    val attribute: ?loc:Location.t -> attribute -> signature_item
  end
module Str:
  sig
    val mk: ?loc:Location.t -> structure_item_desc -> structure_item
    val eval: ?loc:Location.t -> expression -> structure_item
    val value: ?loc:Location.t -> rec_flag -> (pattern * expression) list -> structure_item
    val primitive: ?loc:Location.t -> value_description -> structure_item
    val type_: ?loc:Location.t -> type_declaration list -> structure_item
    val exception_: ?loc:Location.t -> constructor_declaration -> structure_item
    val exn_rebind: ?loc:Location.t -> ?attrs:attribute list -> string loc -> Longident.t loc -> structure_item
    val module_: ?loc:Location.t -> module_binding -> structure_item
    val rec_module: ?loc:Location.t -> module_binding list -> structure_item
    val modtype: ?loc:Location.t -> module_type_binding -> structure_item
    val open_: ?loc:Location.t -> ?attrs:attribute list -> Longident.t loc -> structure_item
    val class_: ?loc:Location.t -> class_declaration list -> structure_item
    val class_type: ?loc:Location.t -> class_type_declaration list -> structure_item
    val include_: ?loc:Location.t -> ?attrs:attribute list -> module_expr -> structure_item
    val extension: ?loc:Location.t -> ?attrs:attribute list -> extension -> structure_item
    val attribute: ?loc:Location.t -> attribute -> structure_item
  end
module Field:
  sig
    val mk: ?loc:Location.t -> core_field_desc -> core_field_type
    val field: ?loc:Location.t -> string -> core_type -> core_field_type
    val var:?loc:Location.t -> unit -> core_field_type
  end
module Cl:
  sig
    val mk: ?loc:Location.t -> class_expr_desc -> class_expr
    val constr: ?loc:Location.t -> Longident.t loc -> core_type list -> class_expr
    val structure: ?loc:Location.t -> class_structure -> class_expr
    val fun_: ?loc:Location.t -> label -> expression option -> pattern -> class_expr -> class_expr
    val apply: ?loc:Location.t -> class_expr -> (label * expression) list -> class_expr
    val let_: ?loc:Location.t -> rec_flag -> (pattern * expression) list -> class_expr -> class_expr
    val constraint_: ?loc:Location.t -> class_expr -> class_type -> class_expr
  end
module Cty:
  sig
    val mk: ?loc:Location.t -> class_type_desc -> class_type
    val constr: ?loc:Location.t -> Longident.t loc -> core_type list -> class_type
    val signature: ?loc:Location.t -> class_signature -> class_type
    val fun_: ?loc:Location.t -> label -> core_type -> class_type -> class_type
  end
module Ctf:
  sig
    val mk: ?loc:Location.t -> class_type_field_desc -> class_type_field
    val inher: ?loc:Location.t -> class_type -> class_type_field
    val val_: ?loc:Location.t -> string -> mutable_flag -> virtual_flag -> core_type -> class_type_field
    val virt: ?loc:Location.t -> string -> private_flag -> core_type -> class_type_field
    val meth: ?loc:Location.t -> string -> private_flag -> core_type -> class_type_field
    val cstr: ?loc:Location.t -> core_type -> core_type -> class_type_field
  end
module Cf:
  sig
    val mk: ?loc:Location.t -> class_field_desc -> class_field
    val inher: ?loc:Location.t -> override_flag -> class_expr -> string option -> class_field
    val valvirt: ?loc:Location.t -> string loc -> mutable_flag -> core_type -> class_field
    val val_: ?loc:Location.t -> string loc -> mutable_flag -> override_flag -> expression -> class_field
    val virt: ?loc:Location.t -> string loc -> private_flag -> core_type -> class_field
    val meth: ?loc:Location.t -> string loc -> private_flag -> override_flag -> expression -> class_field
    val constr: ?loc:Location.t -> core_type -> core_type -> class_field
    val init: ?loc:Location.t -> expression -> class_field
  end
module Val:
  sig
    val mk: ?attrs:attribute list -> ?loc:Location.t -> ?prim:string list -> string loc -> core_type -> value_description
  end
module Mtb:
  sig
    val mk: ?attrs:attribute list -> string loc -> module_type -> module_type_binding
  end
module Md:
  sig
    val mk: ?attrs:attribute list -> string loc -> module_type -> module_declaration
  end
module Mtd:
  sig
    val mk: ?attrs:attribute list -> ?typ:module_type -> string loc -> module_type_declaration
  end
module Mb:
  sig
    val mk: ?attrs:attribute list -> string loc -> module_expr -> module_binding
  end
module Ci:
  sig
    val mk: ?attrs:attribute list -> ?loc:Location.t -> ?virt:virtual_flag -> ?params:(string loc * variance) list * Location.t -> string loc -> 'a -> 'a class_infos
  end
module Type:
  sig
    val mk: ?attrs:attribute list -> ?loc:Location.t -> ?params:(string loc option * variance) list -> ?cstrs:(core_type * core_type * Location.t) list -> ?kind:type_kind -> ?priv:private_flag -> ?manifest:core_type -> string loc -> type_declaration
  end
module Cd:
  sig
    val mk: ?attrs:attribute list -> ?loc:Location.t -> ?args:core_type list -> ?res:core_type -> string loc -> constructor_declaration
  end
module Ld:
  sig
    val mk: ?attrs:attribute list -> ?loc:Location.t -> ?mut:mutable_flag -> string loc -> core_type -> label_declaration
  end
module Csig:
  sig
    val mk: ?loc:Location.t -> core_type -> class_type_field list -> class_signature
  end
