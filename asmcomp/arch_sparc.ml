(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Specific operations for the Sparc processor *)

open Format

type specific_operation = unit          (* None worth mentioning *)

(* Addressing modes *)

type addressing_mode =
    Ibased of string * int              (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)
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
    Ibased(s, n) -> Ibased(s, n + delta)
  | Iindexed n -> Iindexed(n + delta)
  | Iindexed2 n -> Iindexed2(n + delta)

let num_args_addressing = function
    Ibased(s, n) -> 0
  | Iindexed n -> 1
  | Iindexed2 n -> 2

(* Printing operations and addressing modes *)

let print_addressing printreg addr arg =
  match addr with
    Ibased(s, n) ->
      print_string "\""; print_string s; print_string "\"";
      if n <> 0 then begin print_string " + "; print_int n end
  | Iindexed n ->
      printreg arg.(0);
      if n <> 0 then begin print_string " + "; print_int n end
  | Iindexed2 n ->
      printreg arg.(0); print_string " + "; printreg arg.(1);
      if n <> 0 then begin print_string " + "; print_int n end

let print_specific_operation printreg op arg =
  Misc.fatal_error "Arch_sparc.print_specific_operation"
