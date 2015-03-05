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
  | `Int32
  | `Int64
  | `Nativeint
  | `M128
  | `M256
  | `Unit ]

type alternative = {
  mach_register  : [ `all | `a | `b | `c | `d | `S | `D ];
  copy_to_output : int list;
  commutative    : bool;
  earlyclobber   : bool;
  immediate      : bool;
  memory         : [ `no | `m | `m8 | `m16 | `m32 | `m64 | `m128 | `m256 ];
  register       : bool }

type arg = {
  kind         : arg_kind;
  input        : bool;
  output       : bool;
  alternatives : alternative array }

type intrin = {
  asm    : [ `Emit_string of string | `Emit_arg of int ] list;
  args   : arg array;
  cc     : bool;
  memory : bool;
  decl   : string array }

val parse_intrin: arg_kind list -> string list -> intrin

val name : intrin -> string
val description : intrin -> string list
