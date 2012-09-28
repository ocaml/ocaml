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

(* $Id: emitcode.mli 12858 2012-08-10 14:45:51Z maranget $ *)

(* Generation of bytecode for .cmo files *)

open Cmo_format
open Instruct

val to_file: out_channel -> string -> instruction list -> unit
        (* Arguments:
             channel on output file
             name of compilation unit implemented
             list of instructions to emit *)
val to_memory: instruction list -> instruction list ->
                    string * int * (reloc_info * int) list
        (* Arguments:
             initialization code (terminated by STOP)
             function code
           Results:
             block of relocatable bytecode
             size of this block
             relocation information *)
val to_packed_file:
  out_channel -> instruction list -> (reloc_info * int) list
        (* Arguments:
             channel on output file
             list of instructions to emit
           Result:
             relocation information (reversed) *)
