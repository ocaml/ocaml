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

(* Description of intrinsic primitives *)

exception Intrin_error of string

type arg_kind =
  [ `Addr
  | `Float
  | `Int
  | `Int64
  | `M128
  | `M256
  | `Unit ]

type arg = {
  kind           : arg_kind;
  mach_register  : [ `a | `b | `c | `d | `S | `D ] option;
  copy_to_output : int option;
  commutative    : bool;
  earlyclobber   : bool;
  immediate      : bool;
  input          : bool;
  memory         : bool;
  output         : bool;
  register       : bool }

type intrin = {
  asm    : [ `Emit_string of string | `Emit_arg of int ] list;
  args   : arg array;
  cc     : bool;
  memory : bool }

val parse_intrin: arg_kind list -> string list -> intrin

val name : intrin -> string
val description : intrin -> string list
