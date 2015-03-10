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

type register =
  [ `D  | `S  | `a   | `b   | `c   | `d
  | `r8 | `r9 | `r10 | `r11 | `r12 | `r13
  | `x0 | `x1 | `x2  | `x3  | `x4  | `x5  | `x6  | `x7
  | `x8 | `x9 | `x10 | `x11 | `x12 | `x13 | `x14 | `x15 ]

type alternative = {
  mach_register    : [ `all | register ];
  copy_to_output   : int option;
  commutative      : bool;
  disparage        : int;
  earlyclobber     : bool;
  immediate        : bool;
  memory           : [ `no | `m8 | `m16 | `m32 | `m64 | `m128 | `m256 ];
  reload_disparage : int;
  register         : bool }

type arg = {
  kind         : arg_kind;
  input        : bool;
  output       : bool;
  alternatives : alternative array }

type intrin = {
  asm     : [ `Emit_string of string | `Emit_arg of int ] list;
  args    : arg array;
  clobber : [ `cc | `memory | register ] list;
  decl    : string array }

val parse_intrin: arg_kind list -> string list -> intrin

val name : intrin -> string
val description : intrin -> string list
