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

type type_bytes = string
type type_data = Ctype.reified_type_data

type module_type_data

let compare_module_types amty emty =
  amty = emty



type anything
type nothing (* a module, in fact *)

exception Type_error of type_data * type_data
exception Module_type_error of module_type_data * module_type_data

external type_bytes_of : dyn -> type_bytes = "%field0"
let type_of d = (Marshal.from_string (type_bytes_of d) 0 : type_data)
external module_type_of : dynamically_typed_module -> module_type_data = "%field0"

(*
let coerce_internal d expected_type =
  let (sent_type, v) = Obj.magic (d : dyn) in
  if sent_type.expr = expected_type.expr
  then (Obj.magic v : anything)
  else raise (Type_error (sent_type, expected_type))
*)

let coerce_internal d expected_type_bytes =
  let (sent_type_bytes, v : type_bytes * anything) = Obj.magic (d : dyn) in
  let (sent_env, sent_type : type_data) =
    Marshal.from_string sent_type_bytes 0
  and (expected_env, expected_type : type_data) =
    Marshal.from_string expected_type_bytes 0
  in
  Format.pp_print_string Format.err_formatter "%% RECEIVED ";
  Printtyp.type_expr Format.err_formatter sent_type;
  Format.pp_force_newline Format.err_formatter ();
  Format.pp_print_string Format.err_formatter "%% EXPECTED ";
  Printtyp.type_expr Format.err_formatter expected_type;
  Format.pp_force_newline Format.err_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  flush stderr;
  if false
  then v
  else raise (Type_error ((sent_env, sent_type),
                          (expected_env, expected_type)))

let coerce_module d expected_module_type =
  let (actual_module_type, m) = Obj.magic (d : dynamically_typed_module) in
  if compare_module_types actual_module_type expected_module_type
  then (m : nothing)
  else raise (Module_type_error (actual_module_type, expected_module_type))
