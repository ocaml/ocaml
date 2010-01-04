(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Estime, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 2009 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: $ *)

(* Auxiliary used by parsetree and typedtree for value, record label, and
   constructor references. *)

type value_ref =
   | Pvalue of Longident.t
   | Pvalue_ty of Longident.t * string
;;

type constructor_ref =
   | Pconstr of Longident.t
   | Pconstr_ty of Longident.t * string
;;

type label_ref =
   | Plabel of Longident.t
   | Plabel_ty of Longident.t * string
;;
