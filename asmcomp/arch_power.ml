(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Specific operations for the PowerPC processor *)

open Format

type specific_operation =
    Imultaddf                           (* multiply and add *)
  | Imultsubf                           (* multiply and subtract *)

(* Addressing modes *)

type addressing_mode =
    Iindexed of int                     (* reg + displ *)
  | Iindexed2 of int                    (* reg + reg + displ *)

(* Sizes, endianness *)

let big_endian = true

let size_addr = 4
let size_int = 4
let size_float = 8

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
    Iindexed n -> Iindexed(n + delta)
  | Iindexed2 n -> Iindexed2(n + delta)

let num_args_addressing = function
    Iindexed n -> 1
  | Iindexed2 n -> 2

(* Printing operations and addressing modes *)

let print_addressing printreg addr arg =
  match addr with
    Iindexed n ->
      printreg arg.(0);
      if n <> 0 then begin print_string " + "; print_int n end
  | Iindexed2 n ->
      printreg arg.(0); print_string " + "; printreg arg.(1);
      if n <> 0 then begin print_string " + "; print_int n end

let print_specific_operation printreg op arg =
  match op with
    Imultaddf ->
      printreg arg.(0); print_string " *f "; printreg arg.(1);
      print_string " +f "; printreg arg.(2)
  | Imultsubf ->
      printreg arg.(0); print_string " *f "; printreg arg.(1);
      print_string " -f "; printreg arg.(2)
