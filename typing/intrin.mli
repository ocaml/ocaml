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
  kind        : arg_kind;
  cp_to_reg   : [ `No | `Result | `A | `C | `D ];
  reload      : [ `No | `M64 | `M128 | `M256 ];
  immediate   : bool;
  output      : bool;
  register    : bool;
  commutative : bool }

type intrin = {
  asm  : [ `Emit_string of string | `Emit_arg of int ] list;
  args : arg array }

val parse_intrin: arg_kind list -> string list -> intrin

val name : intrin -> string
val description : intrin -> string list
