(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Specific operations for the ARM processor *)

open Misc
open Format

(* Addressing modes *)

type addressing_mode =
    Iindexed of int                     (* reg + displ *)

(* We do not support the reg + shifted reg addressing mode, because
   what we really need is reg + shifted reg + displ,
   and this is decomposed in two instructions (reg + shifted reg -> tmp,
   then addressing tmp + displ). *)

(* Specific operations *)

type specific_operation =
    Ishiftarith of arith_operation * int
  | Ishiftcheckbound of int
  | Irevsubimm of int

and arith_operation =
    Ishiftadd
  | Ishiftsub
  | Ishiftsubrev

(* Sizes, endianness *)

let big_endian = false

let size_addr = 4
let size_int = 4
let size_float = 8

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing (Iindexed n) delta = Iindexed(n + delta)

let num_args_addressing (Iindexed n) = 1

(* Printing operations and addressing modes *)

let print_addressing printreg addr arg =
  match addr with
    Iindexed n ->
      printreg arg.(0);
      if n <> 0 then begin print_string " + "; print_int n end

let print_specific_operation printreg op arg =
  match op with
    Ishiftarith(op, shift) ->
      printreg arg.(0);
      begin match op with
        Ishiftadd -> print_string " + "
      | Ishiftsub -> print_string " - "
      | Ishiftsubrev -> print_string " -rev "
      end;
      printreg arg.(1);
      if shift >= 0
      then begin print_string " << "; print_int shift end
      else begin print_string " >> "; print_int (-shift) end
  | Ishiftcheckbound n ->
      print_string "check ";
      printreg arg.(0);
      print_string " >> "; print_int n; print_string " > ";
      printreg arg.(1)
  | Irevsubimm n ->
      print_int n; print_string " - "; printreg arg.(0)
