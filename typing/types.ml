(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Representation of types and declarations *)

open Misc
open Asttypes

(* Type expressions for the core language *)

type type_expr =
  { mutable desc: type_desc; 
    mutable level: int }

and type_desc =
    Tvar
  | Tarrow of type_expr * type_expr
  | Ttuple of type_expr list
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Tobject of type_expr * (Path.t * type_expr list) option ref
  | Tfield of string * type_expr * type_expr
  | Tnil
  | Tlink of type_expr

and abbrev_memo =
    Mnil
  | Mcons of Path.t * type_expr * abbrev_memo
  | Mlink of abbrev_memo ref

(* Value descriptions *)

type value_description =
  { val_type: type_expr;                (* Type of the value *)
    val_kind: value_kind }

and value_kind =
    Val_reg				(* Regular value *)
  | Val_prim of Primitive.description	(* Primitive *)
  | Val_ivar of mutable_flag		(* Instance variable (mutable ?) *)
  | Val_anc of (string * Ident.t) list  (* Ancestor *)

(* Constructor descriptions *)

type constructor_description =
  { cstr_res: type_expr;                (* Type of the result *)
    cstr_args: type_expr list;          (* Type of the arguments *)
    cstr_arity: int;                    (* Number of arguments *)
    cstr_tag: constructor_tag;          (* Tag for heap blocks *)
    cstr_consts: int;                   (* Number of constant constructors *)
    cstr_nonconsts: int }               (* Number of non-const constructors *)

and constructor_tag =
    Cstr_constant of int                (* Constant constructor (an int) *)
  | Cstr_block of int                   (* Regular constructor (a block) *)
  | Cstr_exception of Path.t            (* Exception constructor *)

(* Record label descriptions *)

type label_description =
  { lbl_res: type_expr;                 (* Type of the result *)
    lbl_arg: type_expr;                 (* Type of the argument *)
    lbl_mut: mutable_flag;              (* Is this a mutable field? *)
    lbl_pos: int;                       (* Position in block *)
    lbl_all: label_description array;   (* All the labels in this type *)
    lbl_repres: record_representation } (* Representation for this record *)

and record_representation =
    Record_regular                      (* All fields are boxed / tagged *)
  | Record_float                        (* All fields are floats *)

(* Type expressions for classes *)

module OrderedString = struct type t = string let compare = compare end
module Vars = Map.Make(OrderedString)
module Concr = Set.Make(OrderedString)

type class_type =
  { cty_params: type_expr list;
    cty_args: type_expr list;
    cty_vars: (Asttypes.mutable_flag * type_expr) Vars.t;
    cty_self: type_expr;
    cty_concr: Concr.t;
    mutable cty_new: type_expr option }

(* Type definitions *)

type type_declaration =
  { type_params: type_expr list;
    type_arity: int;
    type_kind: type_kind;
    type_manifest: type_expr option }

and type_kind =
    Type_abstract
  | Type_variant of (string * type_expr list) list
  | Type_record of (string * mutable_flag * type_expr) list

type exception_declaration = type_expr list

(* Type expressions for the module language *)

type module_type =
    Tmty_ident of Path.t
  | Tmty_signature of signature
  | Tmty_functor of Ident.t * module_type * module_type

and signature = signature_item list

and signature_item =
    Tsig_value of Ident.t * value_description
  | Tsig_type of Ident.t * type_declaration
  | Tsig_exception of Ident.t * exception_declaration
  | Tsig_module of Ident.t * module_type
  | Tsig_modtype of Ident.t * modtype_declaration
  | Tsig_class of Ident.t * class_type

and modtype_declaration =
    Tmodtype_abstract
  | Tmodtype_manifest of module_type

(* Iteration on types *)

let iter_type_expr f ty =
  match ty.desc with
    Tvar               -> ()
  | Tarrow (ty1, ty2) -> f ty1; f ty2
  | Ttuple l           -> List.iter f l
  | Tconstr (_, l, _)          -> List.iter f l
  | Tobject(ty, {contents = Some (_, p)})
                         -> f ty; List.iter f p
  | Tobject (ty, _)    -> f ty
  | Tfield (_, ty1, ty2) -> f ty1; f ty2
  | Tnil               -> ()
  | Tlink ty           -> f ty

(* Memorization of abbreviation expansion *)
(* Must be here rather than in ctype.ml, because used by
   Env.save_signature *)

let memo = ref []
        (* Contains the list of saved abbreviation expansions. *)

let cleanup_abbrev () =
        (* Remove all memorized abbreviation expansions. *)
  List.iter (fun abbr -> abbr := Mnil) !memo;
  memo := []

let memorize_abbrev mem path v =
        (* Memorize the expansion of an abbreviation. *)
  mem := Mcons (path, v, !mem);
  memo := mem :: !memo
