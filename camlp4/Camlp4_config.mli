(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)

val version                      : string;;
val ocaml_standard_library       : string;;
val camlp4_standard_library      : string;;
val ocaml_ast_impl_magic_number  : string;;
val ocaml_ast_intf_magic_number  : string;;
val camlp4_ast_impl_magic_number : string;;
val camlp4_ast_intf_magic_number : string;;
val program_name                 : string ref;;
val unsafe                       : bool ref;;
val verbose                      : bool ref;;
val quotations                   : bool ref;;
val antiquotations               : bool ref;;
val constructors_arity           : bool ref;;
val inter_phrases                : (string option) ref;;
val current_input_file           : string ref;;
