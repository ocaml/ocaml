(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Description of primitive functions *)

type boxed_integer = Pnativeint | Pint32 | Pint64

(* Representation of arguments/result for the native code version
   of a primitive *)
type native_repr =
  | Same_as_ocaml_repr
  | Unboxed_float
  | Unboxed_integer of boxed_integer
  | Untagged_int

type description = private
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)
    prim_native_repr_args: native_repr list;
    prim_native_repr_res: native_repr }

val simple
  :  name:string
  -> arity:int
  -> alloc:bool
  -> description

val parse_declaration
  :  Parsetree.value_description
  -> native_repr_args:native_repr list
  -> native_repr_res:native_repr
  -> description

val description_list_and_attributes
  : description -> string list * string option list

val native_name: description -> string
val byte_name: description -> string

type error =
  | Float_with_native_repr_attribute

exception Error of Location.t * error
