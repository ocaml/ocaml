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

type native_float =
  [ `No           (* No unboxed floats in the inputs and the output *)
  | `Yes          (* Unboxed float inputs and the output *)
  | `Only_input ] (* Unboxed float inputs but not the output *)

type description =
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)
    prim_native_float: native_float }
                               (* Does the above operate on unboxed floats? *)

val parse_declaration: int -> string list -> description

val description_list: description -> string list

val native_name: description -> string
val byte_name: description -> string
