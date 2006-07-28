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

value version                      : string;
value ocaml_standard_library       : string;
value camlp4_standard_library      : string;
value ocaml_ast_impl_magic_number  : string;
value ocaml_ast_intf_magic_number  : string;
value camlp4_ast_impl_magic_number : string;
value camlp4_ast_intf_magic_number : string;
value program_name                 : ref string;
value unsafe                       : ref bool;
value verbose                      : ref bool;
value quotations                   : ref bool;
value constructors_arity           : ref bool;
value inter_phrases                : ref (option string);
value current_input_file           : ref string;
