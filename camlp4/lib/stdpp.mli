(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Stdpp]: standard definitions *)

exception Exc_located of (int * int) and exn;
    (* [Exc_located loc e] is an encapsulation of the exception [e] with
       the input location [loc]. To be used in quotation expanders
       and in grammars to specify some input location for an error.
       Do not raise this exception directly: rather use the following
       function [raise_with_loc]. *)

value raise_with_loc : (int * int) -> exn -> 'a;
    (* [raise_with_loc loc e], if [e] is already the exception [Exc_located],
       re-raise it, else raise the exception [Exc_located loc e]. *)

value loc_name : ref string;
    (* Name of the location variable used in grammars and in the predefined
       quotations for OCaml syntax trees. Default: [loc] *)

