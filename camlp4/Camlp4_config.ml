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

let ocaml_standard_library = Camlp4_import.Config.standard_library;;

let camlp4_standard_library =
  try Sys.getenv "CAMLP4LIB"
  with Not_found -> 
    Filename.concat ocaml_standard_library "camlp4";;

let version = Sys.ocaml_version;;
let program_name = ref "camlp4";;
let constructors_arity = ref true;;
let unsafe             = ref false;;
let verbose            = ref false;;
let antiquotations     = ref false;;
let quotations         = ref true;;
let inter_phrases      = ref None;;
let camlp4_ast_impl_magic_number = "Camlp42006M001";;
let camlp4_ast_intf_magic_number = "Camlp42006N001";;
let ocaml_ast_intf_magic_number = Camlp4_import.Config.ast_intf_magic_number;;
let ocaml_ast_impl_magic_number = Camlp4_import.Config.ast_impl_magic_number;;
let current_input_file = ref "";;
