(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

value fast : ref bool;
value no_constructors_arity : ref bool;
value mkloc : MLast.loc -> Location.t;
value long_id_of_string_list : MLast.loc -> list string -> Longident.t;

value str_item : MLast.str_item -> Parsetree.structure -> Parsetree.structure;
value interf : list MLast.sig_item -> Parsetree.signature;
value implem : list MLast.str_item -> Parsetree.structure;
value phrase : MLast.str_item -> Parsetree.toplevel_phrase;
