(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(*open Globals*)
type global_reference = string
open Primitives

type break_arg =
    BA_none				(* break *)
  | BA_pc of int			(* break PC *)
  | BA_function of global_reference	(* break FUNCTION *)
  | BA_pos1 of string option * int * int option
					(* break @ [MODULE] LINE [POS] *)
  | BA_pos2 of string option * int	(* break @ [MODULE] # OFFSET *)

type pattern =
    P_dummy				(* _ *)
  | P_variable of string		(* x *)
  | P_record of				(* {A = x; ...; D = z} *)
      (global_reference * pattern) list
  | P_list of pattern list		(* [a;,,,;d] *)
  | P_nth of int * pattern		(* # 10 l *)
  | P_concat of pattern * pattern	(* a::l *)
  | P_tuple of pattern list		(* a,...,d *)
  | P_constr of				(* A p *)
      global_reference * pattern	(* > p *)
