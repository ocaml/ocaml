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
    Ibased of string * int              (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)
  | Iindexed2                           (* reg + reg *)

(* Sizes, endianness *)

let big_endian = true

let size_addr = 4
let size_int = 4
let size_float = 8

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
    Ibased(s, n) -> Ibased(s, n + delta)
  | Iindexed n -> Iindexed(n + delta)
  | Iindexed2 -> Misc.fatal_error "Arch_power.offset_addressing"

let num_args_addressing = function
    Ibased(s, n) -> 0
  | Iindexed n -> 1
  | Iindexed2 -> 2

(* Printing operations and addressing modes *)

let print_addressing printreg addr arg =
  match addr with
    Ibased(s, n) ->
      print_string "\""; print_string s; print_string "\"";
      if n <> 0 then begin print_string " + "; print_int n end
  | Iindexed n ->
      printreg arg.(0);
      if n <> 0 then begin print_string " + "; print_int n end
  | Iindexed2 ->
      printreg arg.(0); print_string " + "; printreg arg.(1)

let print_specific_operation printreg op arg =
  match op with
    Imultaddf ->
      printreg arg.(0); print_string " *f "; printreg arg.(1);
      print_string " +f "; printreg arg.(2)
  | Imultsubf ->
      printreg arg.(0); print_string " *f "; printreg arg.(1);
      print_string " -f "; printreg arg.(2)

(* Distinguish between the PowerPC and the Power/RS6000 submodels *)

let powerpc =
  match Config.model with
    "ppc" -> true
  | "rs6000" -> false
  | _ -> Misc.fatal_error "wrong $(MODEL)"

(* Distinguish between the PowerOpen (AIX, MacOS) TOC-based,
   relative-addressing model and the SVR4 (Solaris, MkLinux, Rhapsody)
   absolute-addressing model. *)

let toc =
  match Config.system with
    "aix" -> true
  | "elf" -> false
  | "rhapsody" -> false
  | _ -> Misc.fatal_error "wrong $(SYSTEM)"

