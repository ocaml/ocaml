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

(* Description of inline asm primitives *)

open Inline_asm_arch

exception Inline_asm_error of string

type arg_kind =
  [ `Addr
  | `Float
  | `Int
  | `Int32
  | `Int64
  | `M128d
  | `M256d
  | `M128i
  | `M256i
  | `Nativeint
  | `Unit ]

type alternative = {
  mach_register    : register option;
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

type arg_modifier =
  | None
  | R8L
  | R8H
  | R16
  | R32
  | R64
  | XMM
  | YMM

type template_item =
    Emit_arg of int * arg_modifier
  | Emit_dialect of template array
  | Emit_string of string
  | Emit_unique
and template = template_item array

type inline_asm = {
  template       : template;
  args           : arg array;
  clobber_cc     : bool;
  clobber_memory : bool;
  arch_specifics : arch_specific list;
  decl           : string array }

val parse: arg_kind list -> string list -> inline_asm

val name : inline_asm -> string
val description : inline_asm -> string list
val bytecode_call : inline_asm -> string
